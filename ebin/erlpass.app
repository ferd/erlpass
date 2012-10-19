{application, erlpass,
 [{description, "Safely handle passwords with bcrypt and Erlang"},
  {vsn, "0.1.3"},
  {modules, [erlpass]},
  {applications, [bcrypt]},
  {registered, []},
  {agner, [{requires, ["bcrypt"]}]}
]}.
