# created by: andreas@itship.ch
# content: log command
# @@@@@@@@@@@@@@@@@@@@@@@@@@@

# Global variables
# @@@@@@@@@@@@@@@@@@@@@@@@
# activation and log level:
#   *LogOn - if NIL, no logging happens
#            may define a minimum log level by having one of the following values:
#            ('debug 'info 'warn 'error)
#            if minimum log level is defined, only log messages of that type or above (later in list) are processed
#            if *LogOn is any other value (includes T), logging is presumed as active for all log levels
# output channels:
#   *LogD - Debug-Log output channel
#   *LogI - Information Log output channel
#   *LogW - Warning Log output channel
#   *LogE - Error Log output channel
#   If the corresponding value is NIL, standard output is used.
#   Beside NIL, it should be a filepath starting with "+" (e.g. "+/var/log/pil.log"),
#   or it can be a function returning a filepath (useful to creating a new file (path) after certain size or whatever. Be creative!)
# timestamp flavours:
#   *LogDt - Show date in logging messages (NIL or T)
#   *LogTm - Show time (NIL or T)
#   *LogMs - Show milliseconds (NIL or T)
#   *LogUs - Show microseconds (NIL or T) (only valid when *LogMs is T, else ignored)
#   As the logging is costing time, it isn't an accurate tool for perfomance measurement. Though this might be helpful in finding race conditions.
# PID flavours:
#   *LogPid  - Show process-id in logging messages
#   *LogPPid - If current process is a child, show process-id of parent picolisp process in logging messages
#
# ! By default, no values are defined (so all are NIL) !
#
# Please note, you can locally set this variables to different values, 
# e.g. to log some code parts into different files, or with a another log level and additional information.
# This could be very handy and something which really isn't easily possible in most other languages (pil ftw!)
# @@@@@@@@@@@@@@@@@@@@@@@@

# (log 'Type 'any ...)
# Prints the string representations of any arguments to an logging channel, followed by a newline.
# Logging channel depends on argument Type.
# arguments:
# - Type: one of the following values ('debug 'info 'warn 'error)
#         if Type isn't one of this predefined values, it gets interpreted as part of the message and printed as "LOG" (log level equals 'info")
# - @: a list of strings (like in prinl)
(de log (Type . @)
   (when *LogOn
      (use (Chan Pre TS Msg)
         (when (case Type
                  ("debug"
                     (when (nor (= *LogOn "info") (= *LogOn "warn") (= *LogOn "error"))
                        (setq Chan *LogD)
                        (setq Pre "DEBUG") ) )
                  ("info"
                     (when (nor (= *LogOn "warn") (= *LogOn "error"))
                        (setq Chan *LogI)
                        (setq Pre "INFO") ) )
                  ("warn"
                     (unless (= *LogOn "error")
                        (setq Chan *LogW)
                        (setq Pre "WARN") ) )
                  ("error"
                     (setq Chan *LogE)
                     (setq Pre "ERROR") )
                  (T
                     (when (nor (= *LogOn "warn") (= *LogOn "error"))
                        (setq Chan *LogI)
                        (setq Pre "LOG")
                        (setq Msg Type) ) ) )
            (setq Msg (pack Msg (rest)))
            (setq TS
               (if (and *LogDt *LogTm)
                  (stamp)
                  (if *LogDt
                     (dat$ (date) "-")
                     (when *LogTm
                        (tim$ (time) T) ) ) ) )
            (when *LogMs
               (setq TS (pack TS (when TS ":") (pad 3 (/ (usec T) 1000))))
               (when *LogUs
                  (setq TS (pack TS ":" (pad 3 (% (usec T) 1000)))) ) )
            (when (= (fun? Chan) T)
               (setq Chan (Chan)) )
            (out Chan
               (prinl
                  (when TS (pack TS "|")) 
                  (when (and *LogPid *LogPPid *PPid) (pack *PPid "|"))
                  (when *LogPid (pack *Pid "|"))
                  Pre "|" Msg ) ) ) ) ) )
