{***********************************************************************
Denomo 2.1.0
http://www.kbasm.com/

The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in
compliance with the License.
You may obtain a copy of the License at http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
for the specific language governing rights and limitations under the
License.

The Original Code is DenomoConfig.pas

The Initial Developer of the Original Code is Wang Qi.
***********************************************************************}


unit DenomoConfig;

interface

{=====Configs define start. For detailed document, search doc-ConfigName in this file.}
//All higher priority constants are marked with "!!!"

var
  //!!! IMPORTANT.
  //The directory that contains DenomoRuntime.dll.
  //Maybe you want to change it after you download a new Denomo package.
  //But see the document below for details.
  RuntimeDllDirectory: string = '';

  //usually don't change it unless you renamed the dll.
  RuntimeDllName: string = 'DenomoRuntime.dll';

  KeepTrackOnFreedBlock: Boolean = not True;
  // require KeepTrackOnFreedBlock to be True;
  KeepDetailedTrackOnFreedBlock: Boolean = not True;
  // require KeepDetailedTrackOnFreedBlock to be True;
  DetectVirtualMethodAccssOnFreedObject: Boolean = not True;
  // require KeepDetailedTrackOnFreedBlock to be True;
  DetectInterfaceMethodAccssOnFreedObject: Boolean = not True;

  PromptNoTD32DebugInfo: Boolean = True;

  MaxStringSummaryLen: Integer = 80;

  //!!! see the documents below
  EnableMemTypeString: Boolean = True;
  EnableMemTypeDynArray: Boolean = True;

  MaxSourceInfoCacheTreeNodeCount: Integer = 3000;

  //!!! suggest max to 1, see the documents below
  SkipFramesOfCallingPath: Integer = 0;

  ReportToLogFileWhenCheckOnExit: Boolean = False;
  //0 to overwrite old content with new. Other value is the KBs of the max size.
  LogFileMaxSize: Integer = 0;

  PromptReallocationNotInPlace: Boolean = True;
  PromptRunInReleaseVerion: Boolean = True;

  WarnAnyWay: Boolean = False;
  WarnIfDelphiNotRunning: Boolean = True;
  WarnIfNotBeingDebugged: Boolean = True;
  WarnIfComputerNameNotEqual: string = '';

  //All configs below start with 'Default' are the default values of the options.
  //These options can be changed at runtime in LeakInspector.

  DefaultOutputStackTrace: Boolean = True;
  DefaultOutputSourceInfo: Boolean = True;
  DefaultCheckLeakOnExit: Boolean = True;
  DefaultPromptListTooManyBlock: Boolean = True;
  DefaultOutputStringSummary: Boolean = True;
  DefaultEliminateObjectFieldsLeak: Boolean = True;
  DefaultEliminateLeaksOnSameCallingPath: Boolean = True;

  //The maximum stack trace depth to keep in the memory.
  StackTraceDepth: Integer = 10;

  //The maximum stack trace depth to keep in the memory after the block
  FreedStackTraceDepth: Integer = 10;

  //The maximum stack trace depth to keep in the memory for handle resource.
  HandleStackTraceDepth: Integer = 30;

  //The maximum groups (sessions) can be used in the same time.
  MaxGroupID: Integer = 7;

  // limit detection to current module only
  OnlyReportLeakFromCurrentModule: Boolean = True;

  // used by Denomo for C++, not used by Delphi applications.
  DetectModuleName: string = '';


const
  MaxStackTraceDepth = 32;
  MaxVMTEntryCount = 200;
  MaxInterfaceVMTEntryCount = 200;


{=====Configs define end.}

{=====Config documents start. Don't modify.}

{**************************************************************************
doc-RuntimeDllDirectory
The directory where the runtime dll DenomoRuntime.dll sits in.
You should better copy the dll to the same directory where your program is in
Instead of changing this value to the correct location
That's to say, leaving this constant as blank ('') and put DenomoRuntime.dll
to each your program's output directory. Thus whenever you make the
distribution version of your program and in case you forget remove
Denomo.pas from your project, a "DLL can't be found" error will be raised
so you won't distrubite your program with Denomo in it.
**************************************************************************}

{**************************************************************************
doc-RuntimeDllName
Usually don't change it unless you renamed the dll.
**************************************************************************}

{**************************************************************************
doc-StackTraceDepth
The maximum stack trace depth to keep in the memory.
The bigger it is, the more memory needed for each memory block.
Suggest value: 5 - 10
Default value: 10
**************************************************************************}

{**************************************************************************
doc-FreedStackTraceDepth
The maximum stack trace depth to keep in the memory after the block
is freed.
The bigger it is, the more memory needed for each memory block.
Suggest value: 5 - 10
Default value: 5
**************************************************************************}

{**************************************************************************
doc-MaxGroupID
The maximum groups (sessions) can be used in the same time.
Legal range: 1 - 255
Most time 7 is enough
**************************************************************************}

{**************************************************************************
doc-KeepTrackOnFreedBlock
Enable to keep track on free a block that has been freed.
If this option is enabled, when a block is being freed, that block is not
really freed, instead it's reallocated to a smaller size to keep the
track. Such reallocation requires to be in place, so sometimes you need
to enable UseSimpleMM if the default memory manager can't always resize
a block to smaller size in place.
**************************************************************************}

{**************************************************************************
doc-KeepDetailedTrackOnFreedBlock
Enable to keep more information such as stack trace on freed block.
**************************************************************************}

{**************************************************************************
doc-PromptNoTD32DebugInfo
If no TD32 debug information is detected, whether show a prompt dialog.
This will help when you forget to enable TD32 debug information.
**************************************************************************}

{**************************************************************************
doc-MaxStringSummaryLen
Output the first MaxStringSummaryLen chars of a known string type.
The big this value is, the slower the output will be.
This value only effects when DefaultOutputStringSummary is True.
**************************************************************************}

{**************************************************************************
doc-EnableMemTypeString
Whether recognize the string type.
If this value is True, all ANSI string memory blocks will be recognized and
the string reference count and string summary will be outputed.
If this value is False, all string memory blocks will be marked as raw memory.
**************************************************************************}

{**************************************************************************
doc-EnableMemTypeDynArray
Whether recognize the dynamic array type.
If this value is True, all dynamic array memory blocks will be recognized and
the dynamic array reference count and dimension will be outputed.
If this value is False, all dynamic array memory blocks will be marked as raw memory.
**************************************************************************}

{!!! Important notes on memory type of string and dynamic array.
The mechanism to recognize those two types is heavily depending
on Delphi's internal data structure, so it's possible that recognizing
those types may cause trouble on any un-tested Delphi verison or
future version. If so, just disable those types.
The version of Delphi tested on is Delphi 7, and Delphi 2007. 
}

{**************************************************************************
doc-MaxSourceInfoCacheTreeNodeCount
The maximum source code information to cache.
Default 3000 is enough for most size of application.
3000 meanings at max 3000 addresses of where allocate memory.
If your application is big, e.g more than 100K lines of code,
set the value larger or even MaxInt.
**************************************************************************}

{**************************************************************************
doc-SkipFramesOfCallingPath
The maximum stack frames to skip when checking "same calling path".
The greater the value is, the less accurate the calling path matches.
If the value is 0, the match only occurs when each stack frame exactly same.
If the value is 1, the first stack frames will be skipped even they are not same.
Only set the value to 0 or 1.
**************************************************************************}

{**************************************************************************
doc-ReportToLogFileWhenCheckOnExit
Enable to report leak information to log file when checking on exit.
The log file is same name and location with the program, but the extension
is .denomo.log
**************************************************************************}

{**************************************************************************
doc-LogFileMaxSize
Only valid if ReportToLogFileWhenCheckOnExit is True.
This value is the KB (1024 bytes) size of the maximum log file.
If the value is 0, the content of the log file will be erased and be overwritten
with the new content.
If the log file size is smaller than LogFileMaxSize*1024, new content will
be appended at the end of the file, otherwise, new content will overwrite
the old content.
**************************************************************************}


{**************************************************************************
doc-PromptReallocationNotInPlace
Enable to show a warning message box to prompt that the current used
memory manager can't always do reallocation in place for smaller block size.
Reallocation in place is required to keep track on freed memory blocks.
**************************************************************************}


{**************************************************************************
doc-WarnAnyWay
Enable to show a warning message dialog whenever a program runs with Denomo
enabled.
This is a mechanism a prompt you remember to remove Denomo from
release version. If you feel annoying with this option, turn it off and see
following options.
Only use Denomo in your debug version.
**************************************************************************}

{**************************************************************************
doc-WarnIfDelphiNotRunning
Enable to show a warning message dialog if Delphi is not running.
This is a mechanism a prompt you remember to remove Denomo from
release version.
Only use Denomo in your debug version.
**************************************************************************}

{**************************************************************************
doc-WarnIfNotBeingDebugged
Enable to show a warning message dialog if the program is not started
from within a debugger.
The debugger can be Delphi, or any other debugger.
This is a mechanism a prompt you remember to remove Denomo from
release version.
Only use Denomo in your debug version.
**************************************************************************}

{**************************************************************************
doc-WarnIfComputerNameNotEqual
Enable to show a warning message dialog if current computer name is not
same as the value of this variable. Leave it blank to not to check it.
This is a mechanism a prompt you remember to remove Denomo from
release version.
Only use Denomo in your debug version.
**************************************************************************}


{**************************************************************************
doc-DefaultOutputStackTrace
Whether output the calling stack trace information to LeakInspector.
This will generate more useful output information.
It's highly recommended to always enable this option.
**************************************************************************}

{**************************************************************************
doc-DefaultOutputSourceInfo
Whether output the source code information such as unit name, procedure
name, line number, etc, when outputing the calling stack trace information.
This will generate more useful output information.
It's highly recommended to always enable this option.
**************************************************************************}

{**************************************************************************
doc-DefaultCheckLeakOnExit
Whether reports leaks when the program is exiting.
Just same as many other memory detection tools do.
**************************************************************************}

{**************************************************************************
doc-DefaultPromptListTooManyBlock
Whether show a message box to prompt you when the output is too big.
Outputing too many memory blocks may be very slow.
**************************************************************************}

{**************************************************************************
doc-DefaultOutputStringSummary
Enable to output the first MaxStringSummaryLen chars of a known string type.
Enable this option may slightly slow down the output, depending
on the value of MaxStringSummaryLen
**************************************************************************}

{**************************************************************************
doc-DefaultEliminateObjectFieldsLeak
Whether eliminate unrelated memory leaks to suppress the output information.
Here "unrelated" means the memory blocks allocted in an object's constructor.
Very useful.
**************************************************************************}

{**************************************************************************
doc-DefaultEliminateLeaksOnSameCallingPath
Whether eliminate unrelated memory leaks to suppress the output information.
Here "unrelated" means the memory blocks allocated in the same calling path
(calling stack) of an existing leak.
Very useful.
**************************************************************************}

{**************************************************************************
doc-OnlyReportLeakFromCurrentModule
If it's true, only leaks that created from within current module will be
reported. Otherwise, all leaks will be reported which may cause unnecessary
leak report which caused by the runtime library.
For Delphi users, seems it doesn't matter that if this options is True or False.
For C++ users, always set this option to True.
**************************************************************************}

{**************************************************************************
doc-DetectModuleName
The name of the module to detect leak for.
If detect for current module, leave it empty string.
This is used for Denomo for C++ only.
**************************************************************************}

{=====Config documents end}


implementation

end.

