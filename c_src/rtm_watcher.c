#include <sys/types.h>
#include <sys/socket.h>

#include <netinet/in.h>
#include <arpa/inet.h>

#include <linux/netlink.h>
#include <linux/rtnetlink.h>

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

/* Keep these in sync with the erlang side. */
#define RTM_CMD_ROUTE_ADD   0
#define RTM_CMD_ROUTE_DEL   1
#define RTM_CMD_ROUTE_ERR 255

enum response_data {
    CMD,
    MASK,
    DST,
    GW,
    NUM_RESP
};

#define BUFSIZE 8192

void
parse_attr(struct rtattr *attrs[], struct rtattr *rta, int len)
{
    memset(attrs, 0, sizeof(*rta) * (RTA_MAX + 1));
    while (RTA_OK(rta, len)) {
        if (rta->rta_type <= RTA_MAX)
            attrs[rta->rta_type] = rta;
        rta = RTA_NEXT(rta, len);
    }
}

void
error_reply(char *msg)
{
    int err = RTM_CMD_ROUTE_ERR;
    size_t len = strlen(msg);
    struct iovec iov[3];

    iov[0].iov_base = &err;
    iov[0].iov_len  = 1;
    iov[1].iov_base = &len;
    iov[1].iov_len  = 1;
    iov[2].iov_base = msg;
    iov[2].iov_len  = len;

    writev(STDOUT_FILENO, iov, sizeof(iov)/sizeof(iov[0]));
}

void
error_quit(char *msg)
{
    char buf[BUFSIZE];
    if (errno == 0)
        snprintf(buf, BUFSIZE, "%s", msg);
    else
        snprintf(buf, BUFSIZE, "%s: %s", msg, strerror(errno));
    error_reply(buf);
    exit(1);
}

int
notify(const struct sockaddr_nl *nlp, struct nlmsghdr *nlmsgp)
{
    int len;
    int cmd;
    int host_len;
    struct iovec iov[NUM_RESP];
    struct rtmsg *rtmp;
    struct rtattr *attrs[RTA_MAX + 1];

    switch (nlmsgp->nlmsg_type) {
    case RTM_NEWROUTE:
        cmd = RTM_CMD_ROUTE_ADD;
        break;
    case RTM_DELROUTE:
        cmd = RTM_CMD_ROUTE_DEL;
        break;
    default:
        error_reply("not a route");
        return 1;
    }

    len = nlmsgp->nlmsg_len - NLMSG_LENGTH(sizeof(*nlmsgp));
    if (len < 0) {
        error_reply("wrong message length");
        return 1;
    }

    rtmp = NLMSG_DATA(nlmsgp);
    switch (rtmp->rtm_family) {
    case AF_INET:
        host_len = 4;
        break;
    case AF_INET6:
        host_len = 16;
        break;
    default:
        error_reply("bad message family");
        return 1;
    }

    parse_attr(attrs, RTM_RTA(rtmp), len);

    if (attrs[RTA_GATEWAY] == NULL) {
        error_reply("no gateway");
        return 1;
    }

    iov[CMD].iov_base = &cmd;
    iov[CMD].iov_len  = 1;

    iov[MASK].iov_base = &rtmp->rtm_dst_len;
    iov[MASK].iov_len  = 1;

    if (attrs[RTA_DST] != NULL) {
        iov[DST].iov_base = RTA_DATA(attrs[RTA_DST]);
        iov[DST].iov_len  = (rtmp->rtm_dst_len + 7)/8;
    } else {
        iov[DST].iov_base = '\0';
        iov[DST].iov_len  = 0;
    }

    iov[GW].iov_base = RTA_DATA(attrs[RTA_GATEWAY]);
    iov[GW].iov_len  = host_len;

    writev(STDOUT_FILENO, iov, NUM_RESP);

    return 0;
}

int
bind_socket(void)
{
    int sock;
    int ret;
    struct sockaddr_nl nl;
    int sndbuf = 32768;
    int rcvbuf = 1048576;

    sock = socket(PF_NETLINK, SOCK_RAW, NETLINK_ROUTE);
    if (sock == -1)
        error_quit("socket");

    ret = setsockopt(sock, SOL_SOCKET, SO_SNDBUF, &sndbuf, sizeof(sndbuf));
    if (ret == -1)
        error_quit("setsockopt[SO_SNDBUF]");

    ret = setsockopt(sock, SOL_SOCKET, SO_RCVBUF, &rcvbuf, sizeof(rcvbuf));
    if (ret == -1)
        error_quit("setsockopt[SO_RCVBUF]");

    memset(&nl, 0, sizeof(nl));
    nl.nl_family = AF_NETLINK;
    nl.nl_groups = RTMGRP_LINK
                 | RTMGRP_IPV4_IFADDR | RTMGRP_IPV4_ROUTE
                 | RTMGRP_IPV6_IFADDR | RTMGRP_IPV6_ROUTE;

    ret = bind(sock, (struct sockaddr *)&nl, sizeof(nl));
    if (ret == -1)
        error_quit("bind");

    return sock;
}

void
watch_routes(void)
{
    int sock;
    struct sockaddr_nl nl;
    struct iovec iov;
    struct msghdr msg;
    char buf[BUFSIZE];

    sock = bind_socket();
    
    msg.msg_name = &nl;
    msg.msg_namelen = sizeof(nl);
    msg.msg_iov = &iov;
    msg.msg_iovlen = 1;

    memset(&nl, 0, sizeof(nl));
    nl.nl_family = AF_NETLINK;
    nl.nl_pid = 0;
    nl.nl_groups = 0;

    iov.iov_base = buf;

    for (;;) {
        int ret;
        struct nlmsghdr *p;

        iov.iov_len = BUFSIZE;
        ret = recvmsg(sock, &msg, 0);

        if (ret == -1) {
            if (errno == EINTR || errno == EAGAIN || errno == ENOBUFS)
                continue;
            error_quit("recvmsg");
        }
        if (ret == 0)
            error_quit("recvmsg: EOF");

        if (msg.msg_namelen != sizeof(nl))
            error_quit("bad message namelen");

        p = (struct nlmsghdr *)buf;
        while (ret >= sizeof(*p)) {
            int len = p->nlmsg_len;

            if (len < sizeof(*p) || len > ret) {
                if (msg.msg_flags & MSG_TRUNC)
                    error_quit("truncated message");
                error_quit("malformed message");
            }

            notify(&nl, p);

            ret -= NLMSG_ALIGN(len);
            p = (struct nlmsghdr*)((char*)p + NLMSG_ALIGN(len));
        }
        if (msg.msg_flags & MSG_TRUNC)
            continue;
        if (ret != 0)
            error_quit("unexpected remaining bytes");
    }
}

int
main(int argc, char **argv)
{
    watch_routes();
    /* NOTREACHED */
    return 0;
}
