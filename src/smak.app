% -*- mode: erlang -*-

{application, smak,
 [{description, "smak: an EWGI-based web framework"}
  ,{vsn, "0.2"}
  ,{applications, [kernel, stdlib, ewgi]}
  ,{env, []}
  ,{modules, [smak_auth_basic
              ,smak_auth_cookie
              ,smak_auth_digest
              ,smak_calendar
              ,smak_ewgi
              ,smak_hex
              ,smak_html_util
              ,smak_http_status
              ,smak_random
              ,smak_response
              ,smak_route
              ,smak_sn_cookie
              ,smak_streams
              ,smak_string
              ,smak_url
             ]}
 ]}.
