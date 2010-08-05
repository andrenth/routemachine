{application, routemachine,
 [{description, "RouteMachine"},
  {vsn, "0.0"},
  {modules, [rtm_packager, rtm_packager_sup, rtm_server, rtm_server_sup]},
  {registered, [rtm_packager, rtm_packager_sup, rtm_server, rtm_server_sup]},
  {applications, [kernel, stdlib]},
  {mod, {rtm_app, []}}]}.
