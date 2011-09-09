{application, erlpass,
 [{description, "Safely handle passwords with bcrypt and Erlang"},
  {vsn, "0.0.1"},
  {modules, [erlpass]},
  {applications, [bcrypt]},
  {agner, [{requires, ["bcrypt","proper"]}]}
]}.
