module ObjectIO ( module StdId
             	, module StdIOBasic
             	, module StdIOCommon
             	, module StdKey
             	, module StdGUI
             	, module StdPicture
             	, module StdBitmap
             	, module StdProcess
             	, module StdControl             	
	     	, module StdControlReceiver
             	, module StdReceiver
             	, module StdWindow
             	, module StdMenu
             	, module StdMenuReceiver
             	, module StdMenuElement
             	, module StdTimer
             	, module StdTimerReceiver
             	, module StdFileSelect
             	, module StdSound
             	, module StdClipboard
             	, module StdSystem
             	, liftIO
             	) where

--	********************************************************************************
--	Clean to Haskell Standard Object I/O library, version 1.2
--
--	ObjectIO contains all definition modules of the Object I/O library.
--	********************************************************************************

import StdId			-- The operations that generate identification values
import StdIOBasic		-- Function and type definitions used in the library
import StdIOCommon		-- Function and type definitions used in the library
import StdKey			-- Function and type definitions on keyboard
import StdGUI			-- Type definitions used in the library

import StdPicture
import StdBitmap

import StdProcess		-- Process handling operations

import StdControl		-- Control handling operations
import StdControlReceiver

import StdReceiver		-- Receiver handling operations

import StdWindow		-- Window handling operations

import StdMenu
import StdMenuReceiver
import StdMenuElement

import StdTimer
import StdTimerReceiver

import StdFileSelect

import StdSound

import StdClipboard

import StdSystem

import IOState(liftIO)
