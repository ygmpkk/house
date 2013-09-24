module Main (main) where

--	**************************************************************************************************
--
--	This program creates two interactive processes that communicate via message passing.
--	In a future distributed version this program can be used as a graphical talk application.
--
--	This program is the converted Clean 1.3.2 to Haskell 98 + Object I/O 1.2 version.
--	**************************************************************************************************

import StdIO

--	The message type of talk processes:
data	Message
 =	NewLine String	-- Transmit a line of text
 |	Quit		-- Request termination

--	main creates two talk processes A and B that communicate by means of message passing.
main :: IO ()
main
	= do {
		a     <- openRId;
		b     <- openRId;
		talkA <- talk "A" a b;
		talkB <- talk "B" b a;
		startProcesses [talkA,talkB]
	  }

{-	talk name me you
	defines a talk process named name, to which messages can be sent of type Message
	via me, and that sends messages of type Message to a receiver you.
-}
talk :: String -> RId Message -> RId Message -> IO Process
talk name me you
	= do {
		outId <- openId;
		inId  <- openId;
		let
			infield    = EditControl "" (ContentWidth "mmmmmmmmmm") 5
			                 [ ControlId          inId
			                 , ControlKeyboard    inputfilter Able (input inId you)
			                 ]
			outfield   = EditControl "" (ContentWidth "mmmmmmmmmm") 5
			                 [ ControlId          outId
			                 , ControlPos         (Below inId,zero)
			             --  , ControlSelectState Unable
			                 ]
			talkdialog = Dialog ("Talk "++name) (infield:+:outfield)
			                 [ WindowClose (quit you)
			                 ]
			receiver   = Receiver me (receive outId) []
			process    = Process
			                 NDI
			                 (openDialog talkdialog >> openReceiver receiver >> return ())
			                 [ProcessClose (quit you)]
		in return process
	  }

{-	input handles keyboard input in the input EditControl: 
	for every KeyDown keyboard input that has been accepted by the input EditControl, input sends the 
	current content text of the input EditControl to the other talk process with (NewLine text).
-}
inputfilter :: KeyboardState -> Bool
inputfilter keystate
	= getKeyboardStateKeyState keystate /= KeyUp

input :: Id -> RId Message -> KeyboardState -> GUI ()
input inId you _
	= do {
		Just window <- getParentWindow inId;
		let text = fromJust (snd (getControlText inId window))
		in
		do {
			error <- asyncSend you (NewLine text);
			return ()
		}
	  }
	
{-	The message passing protocol of a talk process.
	On receipt of:
	(1) NewLine text: set the new text to the output control field of the talk dialog.
	(2) Quit:         this is always the last message of the other talk process when termination is 
	                  requested. The process should terminate itself.
-}
receive :: Id -> Message -> GUI ()
receive outId (NewLine text)
	= do {
		setControlText outId text;
	--	setEditControlCursor outId (length text);
	  }
receive _ Quit
	= closeProcess

{-	The quit command first sends the Quit message to the other talk process and then quits itself.
-}	
quit :: RId Message -> GUI ()
quit you
	= do {
		error <- asyncSend you Quit;
		closeProcess
	  }
