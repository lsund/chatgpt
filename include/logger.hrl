-ifndef(CHATGPT_LOGGER_HRL).
-define(CHATGPT_LOGGER_HRL, 1).

-include_lib("kernel/include/logger.hrl").

-define(FORMAT_RECORD(Object, RecordName),
    util:format_record(Object, record_info(fields, RecordName))
).

-endif.
