{application, 'msec', [
	{description, "MySQL/MariaDB Edge Cache"},
	{vsn, ""},
	{modules, ['msec','msec_app','msec_config','msec_metadata','msec_replica','msec_replica_common','msec_replica_mariadb','msec_replica_mysql','msec_replica_sup','msec_resp_dtk','msec_resp_emulator','msec_resp_key','msec_statem','msec_storage','msec_storage_sup','msec_storage_sync','msec_sup','msec_telemetry','msec_telemetry_metrics','msec_telemetry_msc_metrics','msec_telemetry_resp_metrics','msec_util']},
	{registered, [msec_sup]},
	{applications, [kernel,stdlib,ssl,leveled,metrics,msc,resp,rfc4122]},
	{optional_applications, []},
	{mod, {msec_app, []}},
	{env, []}
]}.