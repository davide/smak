%% This is primarily for distribution purposes (e.g. erlware).
{application, smak,
  [{description, "smak: an EWGI-based web framework"},
   {vsn, "0.1.0"},
   {modules, [smak_auth_basic]},
   {registered, []},
   {applications, [kernel, stdlib]},
   {start_phases, []}]}

