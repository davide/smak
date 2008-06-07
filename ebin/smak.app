%% This is primarily for distribution purposes (e.g. erlware).
{application, smak,
 [{description, "smak: an EWGI-based web framework"},
  {vsn, "0.1.0"},
  {modules, [smak_auth_basic,
             smak_auth_digest,
             smak_calendar,
             smak_ewgi,
             smak_hex,
             smak_html_util,
             smak_http_errors,
             smak_random,
             smak_response,
             smak_streams,
             smak_string
            ]},
  {registered, []},
  {applications, [kernel, stdlib]},
  {start_phases, []}]}.

