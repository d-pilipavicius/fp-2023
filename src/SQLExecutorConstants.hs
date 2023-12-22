module SQLExecutorConstants where


import SQLExecutor
import SQLCustomDataTypes (ErrorMessage)

execErrorTemplate :: ErrorMessage -> Executor a
execErrorTemplate err = Executor $ \_ -> Left err 