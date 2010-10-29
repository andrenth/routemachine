#include <sys/types.h>
#include <sys/socket.h>

#include <netinet/in.h>
#include <arpa/inet.h>

#include <linux/netlink.h>
#include <linux/rtnetlink.h>

#include <err.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#define BUFSIZE 8192
#define CMD_LEN 3

int
addattr(struct nlmsghdr *nlmsgp, int maxlen, int type, void *data, int alen)
{
    int len = RTA_LENGTH(alen);
    struct rtattr *rta;

    if (NLMSG_ALIGN(nlmsgp->nlmsg_len) + len > maxlen)
        return -1;

    rta = (struct rtattr*)(((char*)nlmsgp) + NLMSG_ALIGN(nlmsgp->nlmsg_len));
    rta->rta_type = type;
    rta->rta_len = len;
    memcpy(RTA_DATA(rta), data, alen);
    nlmsgp->nlmsg_len = NLMSG_ALIGN(nlmsgp->nlmsg_len) + len;

    return 0;
}

int
send_message(int sock, struct nlmsghdr *nlmsgp)
{
    ssize_t ret;
    int seq, pid;
    struct sockaddr_nl nl;
    struct iovec iov;
    struct msghdr msg;
    char buf[BUFSIZE];
    struct nlmsghdr *p;

    seq = nlmsgp->nlmsg_seq;
    pid = nlmsgp->nlmsg_pid;

    iov.iov_base = nlmsgp;
    iov.iov_len  = nlmsgp->nlmsg_len;

    msg.msg_name    = &nl;
    msg.msg_namelen = sizeof(nl);
    msg.msg_iov     = &iov;
    msg.msg_iovlen  = 1;

    memset(&nl, 0, sizeof(nl));
    nl.nl_family = AF_NETLINK;
    nl.nl_pid    = 0;
    nl.nl_groups = 0;

    ret = sendmsg(sock, &msg, 0);
    if (ret == -1)
        err(1, "sendmsg");

    memset(buf, 0, sizeof(buf));
    iov.iov_base = buf;

    while (1) {
        ret = recvmsg(sock, &msg, 0);
        if (ret <= 0) {
            if (errno == EINTR || errno == EAGAIN)
                continue;
            err(1, "recvmsg");
        }
        if (msg.msg_namelen != sizeof(nl))
            errx(1, "bad message namelen: %d", msg.msg_namelen);

        for (p = (struct nlmsghdr*)buf; ret >= sizeof(*p); ) {
            int len = p->nlmsg_len;

            if ((nlmsgp->nlmsg_seq != seq) || (nlmsgp->nlmsg_pid != pid)) {
                /* Ignore */
                ret -= NLMSG_ALIGN(len);
                p = (struct nlmsghdr*)((char*)p + NLMSG_ALIGN(len));
                continue;
            }

            if (!NLMSG_OK(p, len) || p->nlmsg_type == NLMSG_ERROR) {
                struct nlmsgerr *err = NLMSG_DATA(p);
                if (err->error == 0)
                    return 0;
                errx(1, "read_nl: %s", strerror(-(err->error)));
            }

            if (nlmsgp->nlmsg_type == NLMSG_DONE)
                return 0;
        }

    }
    /* NOTREACHED */
}

int
change_route(int type, int flags, int seq, struct in_addr *dst,
             uint8_t prefix_len, struct in_addr *gw)
{
    int sock;
    char nlmsgbuf[BUFSIZE];
    struct nlmsghdr *nlmsgp;
    struct rtmsg *rtmp;

    sock = socket(PF_NETLINK, SOCK_RAW, NETLINK_ROUTE);
    if (sock == -1)
        err(1, "socket");

    memset(nlmsgbuf, 0, BUFSIZE);
    nlmsgp = (struct nlmsghdr *)nlmsgbuf;

    nlmsgp->nlmsg_len   = NLMSG_LENGTH(sizeof *rtmp);
    nlmsgp->nlmsg_type  = type;
    nlmsgp->nlmsg_flags = NLM_F_REQUEST | NLM_F_ACK | flags;
    nlmsgp->nlmsg_seq   = seq;
    nlmsgp->nlmsg_pid   = getpid();

    rtmp = NLMSG_DATA(nlmsgp);
    rtmp->rtm_family  = AF_INET;
    rtmp->rtm_dst_len = prefix_len;
    rtmp->rtm_src_len = 32;
    rtmp->rtm_tos     = 0;
    rtmp->rtm_table   = RT_TABLE_MAIN;
    rtmp->rtm_flags   = 0;

    switch (type) {
    case RTM_NEWROUTE:
        rtmp->rtm_protocol = RTPROT_BOOT;
        rtmp->rtm_scope    = RT_SCOPE_UNIVERSE;
        rtmp->rtm_type     = RTN_UNICAST;
        break;
    case RTM_DELROUTE:
        rtmp->rtm_scope = RT_SCOPE_NOWHERE;
        break;
    default:
        errx(1, "unsupported netlink message type: %d", type);
    }

    addattr(nlmsgp, BUFSIZE, RTA_GATEWAY, &gw->s_addr, sizeof gw->s_addr);
    addattr(nlmsgp, BUFSIZE, RTA_DST, &dst->s_addr, sizeof dst->s_addr);

    send_message(sock, nlmsgp);

    return 0;
}

int add_route(struct in_addr *dst, uint8_t len, struct in_addr *gw, int seq)
{
    return change_route(RTM_NEWROUTE, NLM_F_EXCL | NLM_F_CREATE, seq,
                        dst, len, gw);
}

int del_route(struct in_addr *dst, uint8_t len, struct in_addr *gw, int seq)
{
    return change_route(RTM_DELROUTE, 0, seq, dst, len, gw);
}

int
main(int argc, char **argv)
{
    uint8_t len;
    struct in_addr dst;
    struct in_addr gw;
    char *cmd;
    int seq = 0;

    if (argc != 5)
        errx(1, "bad argument list");

    cmd = argv[1];
    len = strtol(argv[2], NULL, 10);
    dst.s_addr = htonl(strtoul(argv[3], NULL, 10));
    gw.s_addr  = htonl(strtoul(argv[4], NULL, 10));

    if (strcmp(cmd, "add") == 0)
        add_route(&dst, len, &gw, seq++);
    else if (strcmp(cmd, "del") == 0)
        del_route(&dst, len, &gw, seq++);
    else
        errx(1, "Invalid command: %s", cmd);

    write(STDOUT_FILENO, "", 1);

    return 0;
}
