module MarshalError (
  	module Foreign.Marshal.Error,
#ifndef STANDALONE
	IOErrorType,
	mkIOError,
	alreadyExistsErrorType,
	doesNotExistErrorType,
	alreadyInUseErrorType,
	fullErrorType,
	eofErrorType,
	illegalOperationErrorType,
	permissionErrorType,
	userErrorType,
	annotateIOError
#endif /* ! STANDALONE */
  ) where

#ifndef STANDALONE
import System.IO.Error
#endif /* ! STANDALONE */
import Foreign.Marshal.Error
