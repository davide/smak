% -*- mode: erlang -*-

{application, smak,
 [{description, "smak: an EWGI-based web framework"}
  ,{vsn, "%VSN%"}
  ,{applications, [kernel, stdlib, ewgi]}
  ,{env, []}
  ,{modules, [%MODULES%]}
 ]}.
