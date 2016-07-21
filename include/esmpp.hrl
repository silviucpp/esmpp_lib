
-define(LOG_DEBUG(Format, Args),    lager:debug(Format, Args)).
-define(LOG_INFO(Format, Args),     lager:info(Format, Args)).
-define(LOG_WARNING(Format, Args),  lager:warning(Format, Args)).
-define(LOG_ERROR(Format, Args),    lager:error(Format, Args)).
-define(LOG_CRITICAL(Format, Args), lager:critical(Format, Args)).