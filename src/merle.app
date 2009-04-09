{application, merle,
 [{description, "memcached client, key distribution by ketama"},
  {vsn, "0.4"},
  {modules, [merle, merle_app, merle_sup, ketama]},
  {mod, {merle_app,[]}},
  {registered, [merle_sup]},
  {applications, [kernel, stdlib]}
 ]}.