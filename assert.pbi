; -----------------------------------------------------------------------------
; Copyright (c) 2018 Sean Stasiak. All rights reserved.
; Developed by: Sean Stasiak <sstasiak@protonmail.com>
; Refer to license terms in license.txt; In the absence of such a file,
; contact me at the above email address and I can provide you with one.
; -----------------------------------------------------------------------------

EnableExplicit

Macro ASSERT_DBLQUOTE
  "
EndMacro

Macro assert( exp_ )
  CompilerIf #PB_Compiler_Debugger
  If Not (exp_)
    Debug "ASSERT [F:" + GetFilePart(#PB_Compiler_File) + "][L:" + Str(#PB_Compiler_Line) + "][E:" + ASSERT_DBLQUOTE#exp_#ASSERT_DBLQUOTE + "]"
    CallDebugger
  EndIf
  CompilerEndIf
EndMacro