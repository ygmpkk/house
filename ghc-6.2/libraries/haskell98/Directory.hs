module Directory (
#ifndef STANDALONE
    Permissions( Permissions, readable, writable, executable, searchable ), 
    createDirectory, removeDirectory, removeFile, 
    renameDirectory, renameFile, getDirectoryContents,
    getCurrentDirectory, setCurrentDirectory,
    doesFileExist, doesDirectoryExist,
    getPermissions, setPermissions,
    getModificationTime 
#endif /* ! STANDALONE */
  ) where

#ifndef STANDALONE
import System.Directory
#endif /* ! STANDALONE */
