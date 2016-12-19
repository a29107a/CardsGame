{application, connector_app, [
  {description, "connector app, listens for client connection."},
  {vsn, "1"},
  {registered, []},
  {applications, [
    kernel,
    stdlib,
    ranch
  ]},
  {mod, {connector_app, []}},
  {env, []}
]}.
