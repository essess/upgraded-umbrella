; -----------------------------------------------------------------------------
; Copyright (c) 2018 Sean Stasiak. All rights reserved.
; Developed by: Sean Stasiak <sstasiak@protonmail.com>
; Refer to license terms in license.txt; In the absence of such a file,
; contact me at the above email address and I can provide you with one.
; -----------------------------------------------------------------------------

EnableExplicit

DeclareModule Logger ;< is threadsafe
  
  #MAXWIDTHFILE    = 132   ;< anything longer than maxw gets clipped
  #MAXWIDTHCONSOLE = 80    ;
  
  EnumerationBinary
    #KINDFILE
    #KINDCONSOLE
  EndEnumeration
  
  Declare.i NewLogger( kind.i, name.s="", maxw.i=0 )
  Declare   AtExit( )                   ; finalizes all open iLogger's
  
  Interface iLogger
    LogBegin( )                         ; multi str lock
    LogEnd( )                           ; multi str unlock
    LogStr( str.s )                     ; push string out to logger
    Free.i( *l.iLogger=0 )              ; returns remaining number of references to this iface
 EndInterface
 
; LOG( "<string>" )                     ; reserved macros for modules to define as needed (and turn off logging altogether at compile time)
; LOG_ASSERT( exp )                     ; examples/templates to be provided
; LOG_BEGIN( )
; LOG_END( )
 
  #LOGGERTRACEON = 0
EndDeclareModule

Module Logger ;<<< PRIVATE ----------------------------------------------------
  
  IncludeFile "assert.pbi"  
  
  ;{--- Types -----------------------------------------------------------------
  ;
  ; environment-+->logger
  ;             +->logger
  ;             | ...
  ;             |
  
  Structure tEnv
    mutex.i           ;< lock for atomic environment changes
    start.q
    Map loggers.i()
  EndStructure
  
  Structure tLogger
    *vtbl.iLoggerEx
    *parent.tEnv
    mutex.i           ;< lock for atomic logger access
    maxw.i
    refcnt.i          ;< once refcnt drops to zero, flush/close and delete this logger
  EndStructure
  
  Structure tFileLogger Extends tLogger
    name.s
    file.i
  EndStructure
  
  Structure tConsoleLogger Extends tLogger
  EndStructure
  ;}---------------------------------------------------------------------------
  
  ;{--- Macros ----------------------------------------------------------------
    Macro _TRACE( parent, str )
      CompilerIf #PB_Compiler_Debugger And #LOGGERTRACEON
        Debug "["+prvTimestampStr(*e)+"] "+
              "["+#PB_Compiler_Module+"::"+#PB_Compiler_Procedure+"] "+
              "["+prvPtrToIDStr(parent)+"] < "+
              str
      CompilerEndIf
    EndMacro
    
    Macro _LOCKENV( e )
      _TRACE( e, "LOCK ENV" ) : assert( e\mutex)
      LockMutex( e\mutex )
    EndMacro
    
    Macro _UNLOCKENV( e )
      _TRACE( e, "UNLOCK ENV" ) : assert( e\mutex )
      UnlockMutex( e\mutex )
    EndMacro
    
    Macro _TRACELOGGER( l, str ) 
      _TRACE( l\parent, "[iLogger: "+prvPtrToIDStr(l)+"] "+str )
    EndMacro
    
    Macro _LOCKLOGGER( l )
      _TRACELOGGER( l, "LOCK LOGGER" ) : assert( l\mutex)
      LockMutex( l\mutex )
    EndMacro
    
    Macro _UNLOCKLOGGER( l )
      _TRACELOGGER( l, "UNLOCK LOGGER" ) : assert( l\mutex )
      UnlockMutex( l\mutex )
    EndMacro
    
    Macro _TRACEATTACHMENT( a, str )
      _TRACE( a\parent, "[iAttachment: "+prvPtrToIDStr(a)+"] "+str )
    EndMacro
  ;}---------------------------------------------------------------------------
  
  ;{--- Environment -----------------------------------------------------------  
  Procedure.s prvTimestampStr( *p.tEnv )
    Protected deltams.q  = ElapsedMilliseconds()-(*p\start)
    Protected ms.q       = (deltams/1)     % 1000
    Protected deltasec.q = (deltams/1000)
    Protected seconds.q  = (deltasec/1)    % 60
    Protected minutes.q  = (deltasec/60)   % 60
    Protected hours.q    = (deltasec/3600) % 24
    Protected days.q     = (deltasec/86400)
    ProcedureReturn      Str(days)          +":"+
                    RSet(Str(hours)  ,2,"0")+":"+
                    RSet(Str(minutes),2,"0")+":"+
                    RSet(Str(seconds),2,"0")+":"+
                    RSet(Str(ms)     ,3,"0")
  EndProcedure
  
  Procedure.s prvPtrToIDStr( *p )
    CompilerSelect #PB_Compiler_Processor
      CompilerCase #PB_Processor_x64
        ProcedureReturn "0x"+LSet(Hex(*p,#PB_Quad),16,"0")
      CompilerCase #PB_Processor_x86
        ProcedureReturn "0x"+LSet(Hex(*p,#PB_Long), 8,"0")
    CompilerEndSelect
  EndProcedure
  
  Global *e.tEnv = AllocateStructure( tEnv ) : assert( *e )
  With *e
    \mutex = CreateMutex()
    \start = ElapsedMilliseconds()
  EndWith
  _TRACE( *e, "ENVIRONMENT INITIALIZED" )
  ;}---------------------------------------------------------------------------
    
  ;{--- iLogger ---------------------------------------------------------------  
  Procedure prvLoggerLogBegin( *self.tLogger )
    _TRACELOGGER( *self, "LOCK LOGGER" )
    assert( *self\mutex ) : LockMutex( *self\mutex )
  EndProcedure
  
  Procedure prvLoggerLogEnd( *self.tLogger )
    _TRACELOGGER( *self, "UNLOCK LOGGER" )
    assert( *self\mutex ) : UnlockMutex( *self\mutex )
  EndProcedure  
  
  Procedure prvConsoleLoggerLogStr( *self.tConsoleLogger, str.s )
    _TRACELOGGER( *self, "LOG STRING TO CONSOLE" ) ;< kind of noisy
    str =  Left( "["+prvTimestampStr(*self\parent)+"] "+str, *self\maxw-1 ) : PrintN( str )
    _TRACELOGGER( *self, str )
  EndProcedure
  
  Procedure.i prvConsoleLoggerFree( *self.tConsoleLogger, *lref )
    _TRACELOGGER( *self, "FREE CONSOLE LOGGER" )
    _LOCKENV( *self\parent )
    Protected refcnt.i = *self\refcnt
    ; find self in loggers and remove if *self\refcnt hits 0
    Protected *cl.tConsoleLogger = *self\parent\loggers("CONSOLE")
    If *cl
      assert( *cl = *self )       ;< only one logger per console, and IT BETTER BE THIS ONE
      assert( *self\refcnt > 0 )  ;< because it exists
      *self\refcnt-1 : refcnt = *self\refcnt
      _TRACELOGGER( *self, "REMAINING REFERENCES: "+refcnt )
      If refcnt <= 0                  ;< no more outstanding references
        Protected *l.iLogger = *self  ;< upcast
        *l\LogStr( LSet("CLOSED: "+FormatDate( "%yyyy/%mm/%dd %hh:%ii:%ss ",Date()),*self\maxw,"-") )
        DeleteMapElement( *self\parent\loggers() )  ;< unlink from parent
        FreeMutex( *self\mutex )
        _TRACELOGGER( *self, "POOF" ) : FreeStructure( *self )
      EndIf
      If *lref : PokeI( *lref, 0 ) : EndIf
    Else
      _TRACELOGGER( *self, "LOGGER NOT FOUND IN ENVIRONMENT" )
    EndIf
    _UNLOCKENV( *self\parent )
    ProcedureReturn refcnt
  EndProcedure  
    
  Procedure prvFileLoggerLogStr( *self.tFileLogger, str.s )
    _TRACELOGGER( *self, "LOG STRING TO FILE" ) ;< kind of noisy
    assert( IsFile(*self\file) )
    str =  Left( "["+prvTimestampStr(*self\parent)+"] "+str, *self\maxw-1 )
    If Not WriteStringN( *self\file, str )
      _TRACELOGGER( *self, "WriteStringN() Failed" )
    EndIf
    _TRACELOGGER( *self, str )
  EndProcedure
  
  Procedure.i prvFileLoggerFree( *self.tFileLogger, *lref )
    _TRACELOGGER( *self, "FREE FILE LOGGER" )
    _LOCKENV( *self\parent )
    Protected refcnt.i = *self\refcnt
    ; find self in loggers and remove if *self\refcnt hits 0
    Protected *fl.tFileLogger = *self\parent\loggers(*self\name)
    If *fl
      assert( *fl = *self )
      assert( *self\refcnt > 0 )  ;< because it exists
      *self\refcnt-1 : refcnt = *self\refcnt
      _TRACELOGGER( *self, "REMAINING REFERENCES: "+refcnt )
      If refcnt <= 0                  ;< no more outstanding references
        Protected *l.iLogger = *self  ;< upcast
        *l\LogStr( LSet("CLOSED: "+FormatDate( "%yyyy/%mm/%dd %hh:%ii:%ss ",Date()),*self\maxw,"-") )
        FlushFileBuffers( *fl\file ) : CloseFile( *fl\file )
        DeleteMapElement( *self\parent\loggers() )  ;< unlink from parent
        FreeMutex( *self\mutex )
        _TRACELOGGER( *self, "POOF" ) : FreeStructure( *self )
      EndIf
      If *lref : PokeI( *lref, 0 ) : EndIf
    Else
      _TRACELOGGER( *self, "LOGGER NOT FOUND IN ENVIRONMENT" )
    EndIf
    _UNLOCKENV( *self\parent )
    ProcedureReturn refcnt
  EndProcedure 
  ;}---------------------------------------------------------------------------
  
  ;{--- Classes ---------------------------------------------------------------
  DataSection
    
  iFileLoggerClass:
    Data.i @prvLoggerLogBegin()       ;<-+ is there a way to not duplicate?
    Data.i @prvLoggerLogEnd()         ;<-+
    Data.i @prvFileLoggerLogStr()
    Data.i @prvFileLoggerFree()
    
  iConsoleLoggerClass:
    Data.i @prvLoggerLogBegin()       ;<-+ is there a way to not duplicate?
    Data.i @prvLoggerLogEnd()         ;<-+
    Data.i @prvConsoleLoggerLogStr()
    Data.i @prvConsoleLoggerFree()
    
  EndDataSection
  ;}---------------------------------------------------------------------------  
  
  Procedure.i prvNewFileLogger( *e.tEnv, name.s, maxw.i )
    _LOCKENV( *e )
    Protected *fl.tFileLogger = *e\loggers(name)
    If Not *fl
      _TRACE( *e, "NEW FILE LOGGER" )
      If name = ""
        ; minimize filename collisions
        name = UCase("log_"+FormatDate("%yyyy%mm%dd-%hh%ii%ss_",Date())+
                     LSet(Hex(Random($FFFFFFFF)),8,"0")+".txt")
      EndIf
      *fl = AllocateStructure( tFileLogger )
      If *fl
        With *fl
          \vtbl = ?iFileLoggerClass
          \parent = *e
          \mutex = CreateMutex()
          \maxw = maxw
          \refcnt = 1
          \name = name
          \file = OpenFile( #PB_Any, name, #PB_File_Append )
        EndWith
        If IsFile(*fl\file)
          _TRACE( *e, "FILENAME: "+name )
          FileBuffersSize( *fl\file, 1024*128 )
          *e\loggers(name) = *fl : Protected *l.iLogger = *fl ;< save ref and upcast
          *l\LogStr( LSet("OPENED: "+FormatDate( "%yyyy/%mm/%dd %hh:%ii:%ss ",Date()),*fl\maxw,"-") )
        Else
          FreeStructure( *fl ) : *fl = 0
          _TRACE( *e, "OpenFile() Failed" )
        EndIf
      Else
        _TRACE( *e, "AllocateStructure() Failed" )
      EndIf
    Else
      *fl\refcnt+1
      _TRACE( *e, "EXISTING FILE LOGGER RETURNED" )
    EndIf
    _UNLOCKENV( *e )
    ProcedureReturn *fl
  EndProcedure
  
  Procedure.i prvNewConsoleLogger( *e.tEnv, name.s, maxw.i )
    _LOCKENV( *e )
    Protected *cl.tConsoleLogger = *e\loggers("CONSOLE") 
    If Not *cl ;< no console logger
      _TRACE( *e, "NEW CONSOLE LOGGER" )
      If name = ""
        name = FormatDate( "Created: %hh:%ii:%ss", Date() )
      EndIf
      *cl = AllocateStructure( tConsoleLogger )
      If *cl
        With *cl
          \vtbl = ?iConsoleLoggerClass
          \parent = *e
          \mutex = CreateMutex()
          \maxw = maxw
          \refcnt = 1
        EndWith
        If OpenConsole( name )
          *e\loggers("CONSOLE") = *cl : Protected *l.iLogger = *cl ;< save ref and upcast
          *l\LogStr( LSet("OPENED: "+FormatDate( "%yyyy/%mm/%dd %hh:%ii:%ss ",Date()),*cl\maxw,"-") )
        Else
          FreeStructure( *cl ) : *cl = 0
          _TRACE( *e, "OpenConsole() Failed" )
        EndIf
      Else
        _TRACE( *e, "AllocateStructure() Failed" )
      EndIf
    Else
      *cl\refcnt+1
      _TRACE( *e, "EXISTING CONSOLE LOGGER RETURNED" )
    EndIf
    _UNLOCKENV( *e )
    ProcedureReturn *cl
  EndProcedure
  
  Procedure prvAtExit( *e.tEnv ) ;< done
    _LOCKENV( *e )
    _TRACE( *e, "TEARDOWN EVERYTHING, "+MapSize(*e\loggers())+" LOGGER(S) FOUND" )
    ResetMap( *e\loggers() )
    While NextMapElement( *e\loggers() )
      Protected *l.iLogger = *e\loggers()
      While *l\Free() : Wend ;< Until no more outstanding references against logger
    Wend
    assert( MapSize(*e\loggers())=0 )
    _UNLOCKENV( *e )
  EndProcedure
  
  Procedure.i NewLogger( kind.i, name.s="", maxw.i=0 )
    Protected *l.iLogger = 0
    Select kind
      Case #KINDFILE
        If maxw=0 : maxw=#MAXWIDTHFILE : EndIf
        *l = prvNewFileLogger( *e, name, maxw )
      Case #KINDCONSOLE
        If maxw=0 : maxw=#MAXWIDTHCONSOLE : EndIf
        *l = prvNewConsoleLogger( *e, name, maxw )
      Default
        _TRACE( *e, "KIND UNKNOWN" )
    EndSelect
    ProcedureReturn *l
  EndProcedure
  
  Procedure AtExit( )
    ProcedureReturn prvAtExit( *e )
  EndProcedure
  
  ;{--- Tests -----------------------------------------------------------------
  CompilerIf #PB_Compiler_IsMainFile  ;< tests and usage examples
    
  Procedure Test0( )
    Protected *l.iLogger = NewLogger( #KINDCONSOLE )
    *l\Free( @*l ) : assert( *l=0 )
  EndProcedure    
  
  Procedure Test1( )
    Protected *l.iLogger = NewLogger( #KINDCONSOLE )
    AtExit()
  EndProcedure    
  
  Procedure Test2( )
    Protected *l0.iLogger = NewLogger( #KINDCONSOLE ) : assert( *l0 )
    Protected *l1.iLogger = NewLogger( #KINDCONSOLE ) : assert( *l1=*l0 )
    Protected *l2.iLogger = NewLogger( #KINDCONSOLE ) : assert( *l2=*l1 )
    Protected *l3.iLogger = NewLogger( #KINDCONSOLE ) : assert( *l3=*l2 )
    
    *l0\LogStr( "*l0 log" ) ;< all to same console
    *l1\LogStr( "*l1 log" )
    *l2\LogStr( "*l2 log" )
    *l3\LogStr( "*l3 log" )
    
    *l3\Free( @*l3 ) : assert( *l3=0 )
    *l2\Free( @*l2 ) : assert( *l2=0 )
    *l1\Free( @*l1 ) : assert( *l1=0 )
    *l0\Free( @*l0 ) : assert( *l0=0 )    ;< console closes
    AtExit()                              ;< does nothing (nothing needed)
  EndProcedure 
  
  Procedure Test3( )
    Protected *l0.iLogger = NewLogger( #KINDCONSOLE ) : assert( *l0 )
    Protected *l1.iLogger = NewLogger( #KINDCONSOLE ) : assert( *l1=*l0 )
    Protected *l2.iLogger = NewLogger( #KINDCONSOLE ) : assert( *l2=*l1 )
    Protected *l3.iLogger = NewLogger( #KINDCONSOLE ) : assert( *l3=*l2 )
    ; console still open
    AtExit()
  EndProcedure 
  
  Procedure Test4( )
    DeleteFile( "log.txt" )
    Protected *l.iLogger = NewLogger( #KINDFILE, "log.txt" )
    *l\Free( @*l ) : assert( *l=0 )
  EndProcedure    
  
  Procedure Test5( )
    DeleteFile( "log.txt" )
    Protected *l.iLogger = NewLogger( #KINDFILE, "log.txt" )
    AtExit()
  EndProcedure    
  
  Procedure Test6( )
    DeleteFile( "log.txt" )
    Protected *l0.iLogger = NewLogger( #KINDFILE, "log.txt" ) : assert( *l0 )
    Protected *l1.iLogger = NewLogger( #KINDFILE, "log.txt" ) : assert( *l1=*l0 )
    Protected *l2.iLogger = NewLogger( #KINDFILE, "log.txt" ) : assert( *l2=*l1 )
    Protected *l3.iLogger = NewLogger( #KINDFILE, "log.txt" ) : assert( *l3=*l2 )
    
    *l0\LogStr( "*l0 log" ) ;< all to same file
    *l1\LogStr( "*l1 log" )
    *l2\LogStr( "*l2 log" )
    *l3\LogStr( "*l3 log" )
    
    *l3\Free( @*l3 ) : assert( *l3=0 )
    *l2\Free( @*l2 ) : assert( *l2=0 )
    *l1\Free( @*l1 ) : assert( *l1=0 )
    *l0\Free( @*l0 ) : assert( *l0=0 )    ;< file closes
    AtExit()                              ;< does nothing (nothing needed)
  EndProcedure 
  
  Procedure Test7( )
    DeleteFile( "log.txt" )
    Protected *l0.iLogger = NewLogger( #KINDFILE, "log.txt" ) : assert( *l0 )
    Protected *l1.iLogger = NewLogger( #KINDFILE, "log.txt" ) : assert( *l1=*l0 )
    Protected *l2.iLogger = NewLogger( #KINDFILE, "log.txt" ) : assert( *l2=*l1 )
    Protected *l3.iLogger = NewLogger( #KINDFILE, "log.txt" ) : assert( *l3=*l2 )
    ; file still open
    AtExit()
  EndProcedure 
  
  Procedure Test8( )
    DeleteFile( "log*.txt" )
    Protected *l0.iLogger = NewLogger( #KINDFILE ) : assert( *l0 )
    Protected *l1.iLogger = NewLogger( #KINDFILE ) : assert( *l1 <> *l0 )
    Protected *l2.iLogger = NewLogger( #KINDFILE ) : assert( *l2 <> *l1 )
    Protected *l3.iLogger = NewLogger( #KINDFILE ) : assert( *l3 <> *l2 )
    ; 4 files still open
    AtExit()
  EndProcedure
  
  Procedure Test9( )
    DeleteFile( "log*.txt" )
    Protected *l0.iLogger = NewLogger( #KINDFILE ) : assert( *l0 )
    Protected *l1.iLogger = NewLogger( #KINDFILE ) : assert( *l1 <> *l0 )
    Protected *l2.iLogger = NewLogger( #KINDFILE ) : assert( *l2 <> *l1 )
    Protected *l3.iLogger = NewLogger( #KINDFILE ) : assert( *l3 <> *l2 )
    
    *l3\Free( @*l3 ) : assert( *l3=0 )
    *l2\Free( @*l2 ) : assert( *l2=0 )
    *l1\Free( @*l1 ) : assert( *l1=0 )
    *l0\Free( @*l0 ) : assert( *l0=0 )
    
    AtExit() ;< nothing to do
  EndProcedure
  
  Procedure Test10( )
    Protected *l0.iLogger = NewLogger( #KINDFILE, "log0.txt" ) : assert( *l0 )
    Protected *l1.iLogger = NewLogger( #KINDFILE, "log0.txt" ) : assert( *l1=*l0 )
    Protected *l2.iLogger = NewLogger( #KINDFILE, "log1.txt" ) : assert( *l2 ) : assert( *l2<>*l0 )
    Protected *l3.iLogger = NewLogger( #KINDFILE, "log1.txt" ) : assert( *l3=*l2 )
    
    *l3\Free( @*l3 ) : assert( *l3=0 )
    *l2\Free( @*l2 ) : assert( *l2=0 )
    *l1\Free( @*l1 ) : assert( *l1=0 )
    *l0\Free( @*l0 ) : assert( *l0=0 )
    
    AtExit() ;< nothing to do
  EndProcedure
  
  Procedure Test11( )
    Protected *l0.iLogger = NewLogger( #KINDFILE, "log0.txt" ) : assert( *l0 )
    Protected *l1.iLogger = NewLogger( #KINDFILE, "log0.txt" ) : assert( *l1=*l0 )
    Protected *l2.iLogger = NewLogger( #KINDFILE, "log1.txt" ) : assert( *l2 ) : assert( *l2<>*l0 )
    Protected *l3.iLogger = NewLogger( #KINDFILE, "log1.txt" ) : assert( *l3=*l2 )
    AtExit()
  EndProcedure
  
  Procedure Test12( )
    Protected *l0.iLogger = NewLogger( #KINDFILE, "log0.txt" ) : assert( *l0 )
    Protected *l1.iLogger = NewLogger( #KINDFILE, "log0.txt" ) : assert( *l1=*l0 )
    Protected *l2.iLogger = NewLogger( #KINDFILE, "log1.txt" ) : assert( *l2 ) : assert( *l2<>*l0 )
    Protected *l3.iLogger = NewLogger( #KINDFILE, "log1.txt" ) : assert( *l3=*l2 )
    
    *l3\Free( @*l3 ) : assert( *l3=0 )
    *l2\Free( @*l2 ) : assert( *l2=0 )
    *l1\Free( @*l1 ) : assert( *l1=0 )
    *l0\Free( @*l0 ) : assert( *l0=0 )
    
    *l0 = NewLogger( #KINDFILE, "log0.txt" ) : assert( *l0 )
    *l1 = NewLogger( #KINDFILE, "log0.txt" ) : assert( *l1=*l0 )
    *l2 = NewLogger( #KINDFILE, "log1.txt" ) : assert( *l2 ) : assert( *l2<>*l0 )
    *l3 = NewLogger( #KINDFILE, "log1.txt" ) : assert( *l3=*l2 )
    
    *l3\Free( @*l3 ) : assert( *l3=0 )
    *l2\Free( @*l2 ) : assert( *l2=0 )
    *l1\Free( @*l1 ) : assert( *l1=0 )
    *l0\Free( @*l0 ) : assert( *l0=0 )
    
    *l0 = NewLogger( #KINDFILE, "log0.txt" ) : assert( *l0 )
    *l1 = NewLogger( #KINDFILE, "log0.txt" ) : assert( *l1=*l0 )
    *l2 = NewLogger( #KINDFILE, "log1.txt" ) : assert( *l2 ) : assert( *l2<>*l0 )
    *l3 = NewLogger( #KINDFILE, "log1.txt" ) : assert( *l3=*l2 )
    
    *l3\Free( @*l3 ) : assert( *l3=0 )
    *l2\Free( @*l2 ) : assert( *l2=0 )
    *l1\Free( @*l1 ) : assert( *l1=0 )
    *l0\Free( @*l0 ) : assert( *l0=0 )
    
    AtExit() ;< nothing to do
  EndProcedure
  
  Test0(  ) ; #KINDCONSOLE
  Test1(  ) ; #KINDCONSOLE
  Test2(  ) ; #KINDCONSOLE
  Test3(  ) ; #KINDCONSOLE
  
  Test4(  ) ; #KINDFILE
  Test5(  ) ; #KINDFILE
  Test6(  ) ; #KINDFILE
  Test7(  ) ; #KINDFILE
  
  Test8(  )  ; #KINDFILE
  Test9(  )  ; #KINDFILE
  Test10(  ) ; #KINDFILE
  Test11(  ) ; #KINDFILE
  
  Test12()
  
  CompilerEndIf
  ;}---------------------------------------------------------------------------
 
EndModule