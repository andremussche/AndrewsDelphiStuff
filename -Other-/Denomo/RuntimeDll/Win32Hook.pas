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

The Original Code is Win32Hook.pas

The Initial Developer of the Original Code is Wang Qi.
***********************************************************************}


unit Win32Hook;
////////////////////////////////////////////////////////////////////////////////
//
//   Unit        :  Win32Hook
//   Author      :  rllibby
//   Date        :  11.10.2005
//                  01.30.2006
//   Description :  Unit that is designed to provide tools for function hooking.
//                  A wrapper for IAT hooking, as well as code rewriting is
//                  available. This unit does require DisAsm32 for code rewriting,
//                  which can be downloaded from here:
//
//                  http://users.adelphia.net/~rllibby/downloads/disasm32.zip
//
////////////////////////////////////////////////////////////////////////////////
interface

////////////////////////////////////////////////////////////////////////////////
//   Include Units
////////////////////////////////////////////////////////////////////////////////
uses
  Windows, SysUtils, Classes, DisAsm32;

////////////////////////////////////////////////////////////////////////////////
//   Win32Hook data types
////////////////////////////////////////////////////////////////////////////////
type
  IndirectAddr      =  ^Pointer;

////////////////////////////////////////////////////////////////////////////////
//   Win32Hook constants
////////////////////////////////////////////////////////////////////////////////
const
  MAX_LIBRARYNAME   =  MAX_PATH;
  MAX_FUNCTIONNAME  =  255;
  MIN_INSTRSIZE     =  5;

////////////////////////////////////////////////////////////////////////////////
//   Win32Hook record structures
////////////////////////////////////////////////////////////////////////////////
type
  PImageImportDesc  =  ^TImageImportDesc;
  TImageImportDesc  =  packed record
     FuncNameList:  DWORD;
     TimeDateStamp: DWORD;
     ForwarderChain:DWORD;
     Name:          DWORD;
     FirstThunk:    DWORD;
  end;

  PHintTable        =  ^THintTable;
  THintTable        =  packed record
     FuncName:      DWORD;
  end;

  PIATListEntry     =  ^TIATListEntry;
  TIATListEntry     =  packed record
     IAT:           IndirectAddr;
     Next:          PIATListEntry;
  end;

  PPatchRecord      =  ^TPatchRecord;
  TPatchRecord       =  packed record
     LibraryName:   Array [0..MAX_LIBRARYNAME] of Char;
     FunctionName:  Array [0..MAX_FUNCTIONNAME] of Char;
     FunctionAddr:  Pointer;
     IATList:       PIATListEntry;
  end;

  PLibRemote        =  ^TLibRemote;
  TLibRemote        =  packed record
     ProcessID:     DWORD;
     LibraryName:   Array [0..MAX_LIBRARYNAME] of Char;
     LibraryHandle: HMODULE;
  end;

  PJumpBlock        =  ^TJumpBlock;
  TJumpBlock        =  packed record
     Code:          Byte;
     Offset:        Integer;
  end;

  PRewriteRecord    =  ^TRewriteRecord;
  TRewriteRecord    =  packed record
     BaseAddress:   Pointer;
     LinkAddress:   PChar;
     CodeSize:      DWORD;
  end;

////////////////////////////////////////////////////////////////////////////////
//   Win32 code hook rewriting class
////////////////////////////////////////////////////////////////////////////////
type
  TCodeRewrite      =  class(TObject)
  private
     // Private decalations
     FLock:         TRTLCriticalSection;
     FRewriteList:  TList;
     FUnlink:       Boolean;
    FHardCore: Boolean;
     function       GetInstructionSize(FunctionAddress: Pointer): Integer;
     procedure      InternalUnlink(RewriteRecord: PRewriteRecord);
  protected
     // Protected declarations
     function       GetCount: Integer;
     function       GetRewriteRecord(Index: Integer): PRewriteRecord;
     function       GetUnlinkOnFree: Boolean;
     procedure      SetUnlinkOnFree(Value: Boolean);
  public
     // Public declarations
     constructor    Create;
     destructor     Destroy; override;
     procedure      Clear(Unlink: Boolean);
     procedure      Delete(Index: Integer; Unlink: Boolean);
     function       Find(FunctionAddress: Pointer): PRewriteRecord;
     function       Link(FunctionAddress: Pointer; NewFunction: Pointer; out OldFunction: Pointer): Boolean;
     procedure      Unlink(FunctionAddress: Pointer);
     property       Count: Integer read GetCount;
     property       Items[Index: Integer]: PRewriteRecord read GetRewriteRecord; default;
     property       UnlinkOnFree: Boolean read GetUnlinkOnFree write SetUnlinkOnFree;

     property       HardCore: Boolean read FHardCore write FHardCore;
  end;

////////////////////////////////////////////////////////////////////////////////
//   Win32 library injection class (NT/2000/XP/2003)
////////////////////////////////////////////////////////////////////////////////
type
  TLibraryInject    =  class(TObject)
  private
     // Private declarations
     FLock:         TRTLCriticalSection;
     FInjectList:   TList;
     FUnload:       Boolean;
  protected
     // Protected declarations
     function       GetCount: Integer;
     function       GetLibRemote(Index: Integer): PLibRemote;
     function       GetUnloadOnFree: Boolean;
     procedure      SetUnloadOnFree(Value: Boolean);
  public
     // Public declarations
     constructor    Create;
     destructor     Destroy; override;
     function       Add(ProcessID: DWORD; LibraryName: String): Integer;
     procedure      Clear(Unload: Boolean);
     procedure      Delete(Index: Integer; Unload: Boolean);
     property       Count: Integer read GetCount;
     property       Items[Index: Integer]: PLibRemote read GetLibRemote;
     property       UnloadOnFree: Boolean read GetUnloadOnFree write SetUnloadOnFree;
  end;

////////////////////////////////////////////////////////////////////////////////
//   Win32 IAT patching class
////////////////////////////////////////////////////////////////////////////////
type
  TImportAddress    =  class(TObject)
  private
     // Private decalations
     FLock:         TRTLCriticalSection;
     FPatchList:    TList;
     FUnpatch:      Boolean;
     procedure      BuildIATPatchList(LibraryName, FunctionName: String; var List: PIATListEntry);
     function       CompareLibraryNames(Name1, Name2: String): Boolean;
     procedure      EnumIATList(Module: HMODULE; LibraryName, FunctionName: String; var List: PIATListEntry);
     function       ListContainsIAT(List: PIATListEntry; IAT: IndirectAddr): Boolean;
     function       NextFunctionName(lpName: PChar): PChar;
     function       UpdatePatch(PatchRecord: PPatchRecord; FunctionAddress: Pointer): Boolean;
     function       UpdateEntry(Entry: PIATListEntry; FunctionAddress: Pointer): Boolean;
  protected
     // Protected declarations
     function       GetCount: Integer;
     function       GetPatchRecord(Index: Integer): PPatchRecord;
     function       GetUnpatchOnFree: Boolean;
     procedure      SetUnpatchOnFree(Value: Boolean);
  public
     // Public declarations
     constructor    Create;
     destructor     Destroy; override;
     procedure      Clear(Unpatch: Boolean);
     procedure      Delete(Index: Integer; Unpatch: Boolean);
     function       Find(LibraryName, FunctionName: String): PPatchRecord;
     function       Patch(LibraryName, FunctionName: String; NewFunction: Pointer; out OldFunction: Pointer): Boolean;
     procedure      Unpatch(LibraryName, FunctionName: String);
     property       Count: Integer read GetCount;
     property       Items[Index: Integer]: PPatchRecord read GetPatchRecord; default;
     property       UnpatchOnFree: Boolean read GetUnpatchOnFree write SetUnpatchOnFree;
  end;

implementation

//// TCodeRewrite //////////////////////////////////////////////////////////////
function TCodeRewrite.GetInstructionSize(FunctionAddress: Pointer): Integer;
var  daCode:        TDisAsm;
     dwIndex:       Integer;
begin

  // Set default result
  result:=0;

  // Create disassembler class
  daCode:=TDisAsm.Create;

  // Resource protection
  try
     // Decompile a minimum of 5 bytes
     daCode.Disassemble(FunctionAddress, MIN_INSTRSIZE);
     // Walk the disassembled instructions
     for dwIndex:=0 to Pred(daCode.Count) do
     begin
        // Check the op code type
        if (not HardCore)
          and (((daCode.Instructions[dwIndex].Code.mnemType-INS_EXEC) in [1..6])) then
        begin
           // Can't safely deal with this
           result:=0;
           // Done processing
           break;
        end
        else
           // Increment the result by the op code instruction size
           Inc(result, daCode.Instructions[dwIndex].Code.size);
     end;
  finally
     // Free the disassembler
     daCode.Free;
  end;

end;

function TCodeRewrite.Link(FunctionAddress: Pointer; NewFunction: Pointer; out OldFunction: Pointer): Boolean;
var  lpRewrite:     PRewriteRecord;
     lpJump:        TJumpBlock;
     lpLink:        PJumpBlock;
     dwProtect:     DWORD;
     dwSize:        Integer;
begin

  // Enter critical section
  EnterCriticalSection(FLock);

  // Resource protection
  try
     // Set default result
     result:=False;
     // Check OS and function address pointer to rewrite with our JMP code
     if Assigned(FunctionAddress) and Assigned(NewFunction) and (((GetVersion and $80000000) = 0) or (DWORD(FunctionAddress) < $80000000)) then
     begin
        // Check to see if this has already been hooked
        lpRewrite:=Find(FunctionAddress);
        // Check assignment
        if Assigned(lpRewrite) then
        begin
           // Simple code update by setting the new function pointer
           lpJump.Code:=$E9;
           lpJump.Offset:=Integer(NewFunction)-(Integer(FunctionAddress)+SizeOf(TJumpBlock));
           // Set memory protection so we can rewrite the code
           if VirtualProtect(FunctionAddress, SizeOf(TJumpBlock), PAGE_EXECUTE_READWRITE, dwProtect) then
           begin
              // Update the function with the jump code
              Move(lpJump, FunctionAddress^, SizeOf(TJumpBlock));
              // Flush the instruction cache
              FlushInstructionCache(GetCurrentProcess, nil, 0);
              // Set the old function pointer (linked code we created)
              OldFunction:=lpRewrite^.LinkAddress;
              // Success
              result:=True;
           end;
        end
        else
        begin
           // Get the size that we will need to move from the original function address
           dwSize:=GetInstructionSize(FunctionAddress);
           // Verify that the size if valid
           if (dwSize >= MIN_INSTRSIZE) then
           begin
              // Set memory protection so we can rewrite the code
              if VirtualProtect(FunctionAddress, SizeOf(TJumpBlock), PAGE_EXECUTE_READWRITE, dwProtect) then
              begin
                 // Create the rewrite record to store the information
                 lpRewrite:=AllocMem(SizeOf(TRewriteRecord));
                 // Resource protection
                 try
                    // Update the record info
                    lpRewrite^.BaseAddress:=FunctionAddress;
                    lpRewrite^.CodeSize:=DWORD(dwSize);
                    // Allocate virtual memory for the linking
                    lpRewrite^.LinkAddress:=VirtualAlloc(nil, dwSize+SizeOf(TJumpBlock), MEM_COMMIT, PAGE_EXECUTE_READWRITE);
                    // Copy instructions from the function
                    Move(FunctionAddress^, lpRewrite^.LinkAddress^, dwSize);
                    // Get pointer for start of jump code
                    lpLink:=@lpRewrite^.LinkAddress[dwSize];
                    // Update the linked jump
                    lpLink^.Code:=$E9;
                    lpLink^.Offset:=(Integer(FunctionAddress) + dwSize) - (Integer(lpLink) + SizeOf(TJumpBlock));
                    // Code update by setting the new function pointer
                    lpJump.Code:=$E9;
                    lpJump.Offset:=Integer(NewFunction) - (Integer(FunctionAddress) + SizeOf(TJumpBlock));
                    // Update the function with the jump code
                    Move(lpJump, FunctionAddress^, SizeOf(TJumpBlock));
                    // Flush the instruction cache
                    FlushInstructionCache(GetCurrentProcess, nil, 0);
                    // Set the old function pointer (linked code we created)
                    OldFunction:=lpRewrite^.LinkAddress;
                    // Success
                    result:=True;
                 finally
                    // Add record to list
                    FRewriteList.Add(lpRewrite);
                 end;
              end;
           end;
        end;
     end;
  finally
     // Leave the critical section
     LeaveCriticalSection(FLock);
  end;

end;

procedure TCodeRewrite.InternalUnlink(RewriteRecord: PRewriteRecord);
var  dwProtect:     DWORD;
begin

  // Check rewrite record
  if Assigned(RewriteRecord) and Assigned(RewriteRecord^.BaseAddress) then
  begin
     // Set memory protection so we can set the original code back
     if VirtualProtect(RewriteRecord^.BaseAddress, RewriteRecord^.CodeSize, PAGE_EXECUTE_READWRITE, dwProtect) then
     begin
        // Move the original code back
        Move(RewriteRecord^.LinkAddress^, RewriteRecord^.BaseAddress^, RewriteRecord^.CodeSize);
        // Clear the base address
        RewriteRecord^.BaseAddress:=nil;
        // Resource protection
        try
           // Free the linked memory
           VirtualFree(RewriteRecord^.LinkAddress, 0, MEM_RELEASE);
           // Reset the code size
           RewriteRecord^.CodeSize:=0;
        finally
           // Clear the pointer
           RewriteRecord^.LinkAddress:=nil;
        end;
     end;
  end;

end;

procedure TCodeRewrite.Unlink(FunctionAddress: Pointer);
var  lpRewrite:     PRewriteRecord;
begin

  // Enter critical section
  EnterCriticalSection(FLock);

  // Resource protection
  try
     // Attempt to find the function address in the rewrite list
     lpRewrite:=Find(FunctionAddress);
     // Check find result
     if Assigned(lpRewrite) then
     begin
        // Remove from list
        FRewriteList.Remove(lpRewrite);
        // Resource protection
        try
           // Perform the internal unlink
           InternalUnlink(lpRewrite);
        finally
           // Free memory for rewrite record
           FreeMem(lpRewrite);
        end;
     end;
  finally
     // Leave the critical section
     LeaveCriticalSection(FLock);
  end;

end;

function TCodeRewrite.Find(FunctionAddress: Pointer): PRewriteRecord;
var  dwIndex:       Integer;
begin

  // Enter critical section
  EnterCriticalSection(FLock);

  // Resource protection
  try
     // Set default result
     result:=nil;
     // Walk the rewrite record list
     for dwIndex:=0 to Pred(FRewriteList.Count) do
     begin
        // Check against base pointer address for the rewrite record
        if (PRewriteRecord(FRewriteList[dwIndex])^.BaseAddress = FunctionAddress) then
        begin
           // Found the record
           result:=FRewriteList[dwIndex];
           // Done processing
           break;
        end;
     end;
  finally
     // Leave the critical section
     LeaveCriticalSection(FLock);
  end;

end;

procedure TCodeRewrite.Delete(Index: Integer; Unlink: Boolean);
var  lpRewrite:     PRewriteRecord;
begin

  // Enter critical section
  EnterCriticalSection(FLock);

  // Resource protection
  try
     // Get the rewrite record at the given index
     lpRewrite:=FRewriteList[Index];
     // Resource protection
     try
        // Delete from the rewrite list
        FRewriteList.Delete(Index);
        // Do we need to unlink?
        if Unlink then InternalUnlink(lpRewrite);
     finally
        // Free the rewrite record memory
        FreeMem(lpRewrite);
     end;
  finally
     // Leave critical section
     LeaveCriticalSection(FLock);
  end;

end;

procedure TCodeRewrite.Clear(Unlink: Boolean);
var  dwIndex:       Integer;
begin

  // Enter critical section
  EnterCriticalSection(FLock);

  // Resource protection
  try
     // Resource protection
     try
        // Walk the rewrite list
        for dwIndex:=Pred(FRewriteList.Count) downto 0 do
        begin
           // Delete and optionally unlink the rewrite
           Delete(dwIndex, Unlink);
        end;
     finally
        // Clear the rewrite list
        FRewriteList.Clear;
     end;
  finally
     // Leave critical section
     LeaveCriticalSection(FLock);
  end;

end;

function TCodeRewrite.GetRewriteRecord(Index: Integer): PRewriteRecord;
begin

  // Enter critical section
  EnterCriticalSection(FLock);

  // Resource protection
  try
     // Return the rewrite record at the given index
     result:=FRewriteList[Index];
  finally
     // Leave critical section
     LeaveCriticalSection(FLock);
  end;

end;

function TCodeRewrite.GetUnlinkOnFree: Boolean;
begin

  // Enter critical section
  EnterCriticalSection(FLock);

  // Resource protection
  try
     // Return the value
     result:=FUnlink;
  finally
     // Leave critical section
     LeaveCriticalSection(FLock);
  end;

end;

procedure TCodeRewrite.SetUnlinkOnFree(Value: Boolean);
begin

  // Enter critical section
  EnterCriticalSection(FLock);

  // Resource protection
  try
     // Update the value
     FUnlink:=Value;
  finally
     // Leave critical section
     LeaveCriticalSection(FLock);
  end;

end;

function TCodeRewrite.GetCount: Integer;
begin

  // Enter critical section
  EnterCriticalSection(FLock);

  // Resource protection
  try
     // Return the count of rewrite records
     result:=FRewriteList.Count;
  finally
     // Leave critical section
     LeaveCriticalSection(FLock);
  end;

end;

constructor TCodeRewrite.Create;
begin

  // Perform inherited
  inherited Create;

  // Create critical section
  InitializeCriticalSection(FLock);

  // Create list for holding rewrite information
  FRewriteList:=TList.Create;

  // Set default unlink on free setting to false
  FUnlink:=False;

end;

destructor TCodeRewrite.Destroy;
begin

  // Resource protection
  try
     // Resource protection
     try
        // Clear the rewrite list
        Clear(FUnlink);
     finally
        // Free the rewrite list
        FRewriteList.Free;
     end;
     // Delete the critical section
     DeleteCriticalSection(FLock);
  finally
     // Perform inherited
     inherited Destroy;
  end;

end;

//// TLibraryInject ////////////////////////////////////////////////////////////
function TLibraryInject.GetCount: Integer;
begin

  // Enter critical section
  EnterCriticalSection(FLock);

  // Resource protection
  try
     // Return the count of injected remote libraries
     result:=FInjectList.Count;
  finally
     // Leave critical section
     LeaveCriticalSection(FLock);
  end;

end;

function TLibraryInject.GetLibRemote(Index: Integer): PLibRemote;
begin

  // Enter critical section
  EnterCriticalSection(FLock);

  // Resource protection
  try
     // Return the injected remote library record
     result:=PLibRemote(FInjectList[Index]);
  finally
     // Leave critical section
     LeaveCriticalSection(FLock);
  end;

end;

function TLibraryInject.GetUnloadOnFree: Boolean;
begin

  // Enter critical section
  EnterCriticalSection(FLock);

  // Resource protection
  try
     // Return the property setting
     result:=FUnload;
  finally
     // Leave critical section
     LeaveCriticalSection(FLock);
  end;

end;

procedure TLibraryInject.SetUnloadOnFree(Value: Boolean);
begin

  // Enter critical section
  EnterCriticalSection(FLock);

  // Resource protection
  try
     // Update the property setting
     FUnload:=Value;
  finally
     // Leave critical section
     LeaveCriticalSection(FLock);
  end;

end;

function TLibraryInject.Add(ProcessID: DWORD; LibraryName: String): Integer;
var  hKernel:       HMODULE;
     hProcess:      THandle;
     hThread:       THandle;
     dwNull:        Cardinal;
     lpRemote:      PLibRemote;
     lpLibRemote:   PChar;
begin

  // Enter critical section
  EnterCriticalSection(FLock);

  // Resource protection
  try
     // Set default result of (-1), which means the injection failed
     result:=(-1);
     // Check library name and version of OS we are running on
     if (Length(LibraryName) > 0) and ((GetVersion and $80000000) = 0)then
     begin
        // Attempt to open the process
        hProcess:=OpenProcess(PROCESS_ALL_ACCESS, False, ProcessID);
        // Check process handle
        if (hProcess <> 0) then
        begin
           // Resource protection
           try
              // Get module handle for kernel32
              hKernel:=GetModuleHandle('kernel32');
              // Check handle
              if (hKernel <> 0) then
              begin
                 // Allocate memory in other process
                 lpLibRemote:=VirtualAllocEx(hProcess, nil, Succ(Length(LibraryName)), MEM_COMMIT, PAGE_READWRITE);
                 // Check memory pointer
                 if Assigned(lpLibRemote) then
                 begin
                    // Resource protection
                    try
                       // Write the library name to the memory in other process
                       WriteProcessMemory(hProcess, lpLibRemote, PChar(LibraryName), Length(LibraryName), dwNull);
                       // Create the remote thread
                       hThread:=CreateRemoteThread(hProcess, nil, 0, GetProcAddress(hKernel, 'LoadLibraryA'), lpLibRemote, 0, dwNull);
                       // Check the thread handle
                       if (hThread <> 0) then
                       begin
                          // Resource protection
                          try
                             // Allocate a new remote injection record
                             lpRemote:=AllocMem(SizeOf(TLibRemote));
                             // Set process id
                             lpRemote^.ProcessID:=ProcessID;
                             // Copy library name
                             StrPLCopy(lpRemote^.LibraryName, LibraryName, MAX_LIBRARYNAME);
                             // Wait for the thread to complete
                             WaitForSingleObject(hThread, INFINITE);
                             // Fill in the library handle
                             GetExitCodeThread(hThread, DWORD(lpRemote^.LibraryHandle));
                             // Add to list
                             result:=FInjectList.Add(lpRemote);
                          finally
                             // Close the thread handle
                             CloseHandle(hThread);
                          end;
                       end;
                    finally
                       // Free allocated memory
                       VirtualFree(lpLibRemote, 0, MEM_RELEASE);
                    end;
                 end;
              end;
           finally
              // Close the process handle
              CloseHandle(hProcess);
           end;
        end;
     end;
  finally
     // Leave critical section
     LeaveCriticalSection(FLock);
  end;

end;

procedure TLibraryInject.Clear(Unload: Boolean);
var  dwIndex:       Integer;
begin

  // Enter critical section
  EnterCriticalSection(FLock);

  // Resource protection
  try
     // Resource protection
     try
        // Walk the list and delete each record
        for dwIndex:=Pred(FInjectList.Count) downto 0 do
        begin
           // Delete and optionally unload the remote library
           Delete(dwIndex, Unload);
        end;
     finally
        // Clear the list
        FInjectList.Clear;
     end;
  finally
     // Leave critical section
     LeaveCriticalSection(FLock);
  end;

end;

procedure TLibraryInject.Delete(Index: Integer; Unload: Boolean);
var  hProcess:      THandle;
     hThread:       THandle;
     hKernel:       HMODULE;
     dwNull:        Cardinal;
     lpRemote:      PLibRemote;
begin

  // Enter critical section
  EnterCriticalSection(FLock);

  // Resource protection
  try
     // Get the remote library record
     lpRemote:=FInjectList[Index];
     // Resource protection
     try
        // Do we need to unload?
        if Unload then
        begin
           // Attempt to open the process
           hProcess:=OpenProcess(PROCESS_ALL_ACCESS, False, lpRemote^.ProcessID);
           // Check process handle
           if (hProcess <> 0) then
           begin
              // Resource protection
              try
                 // Get module handle for kernel32
                 hKernel:=GetModuleHandle('kernel32');
                 // Check handle
                 if (hKernel <> 0) then
                 begin
                    // Create the remote thread
                    hThread:=CreateRemoteThread(hProcess, nil, 0, GetProcAddress(hKernel, 'FreeLibrary'), Pointer(lpRemote^.LibraryHandle), 0, dwNull);
                    // Check the thread handle
                    if (hThread <> 0) then
                    begin
                       // Resource protection
                       try
                          // Wait for the thread to complete
                          WaitForSingleObject(hThread, INFINITE);
                       finally
                          // Close the thread handle
                          CloseHandle(hThread);
                       end;
                    end;
                 end;
              finally
                 // Close the process handle
                 CloseHandle(hProcess);
              end;
           end;
        end;
        // Free memory
        FreeMem(lpRemote);
     finally
        // Remove from the list
        FInjectList.Delete(Index);
     end;
  finally
     // Leave critical section
     LeaveCriticalSection(FLock);
  end;

end;

constructor TLibraryInject.Create;
begin

  // Perform inherited
  inherited Create;

  // Create critical section
  InitializeCriticalSection(FLock);

  // Create list for holding injection information
  FInjectList:=TList.Create;

  // Set default unload on free setting to false
  FUnload:=False;

end;

destructor TLibraryInject.Destroy;
begin

  // Resource protection
  try
     // Resource protection
     try
        // Clear the injection list
        Clear(FUnload);
     finally
        // Free the injection list
        FInjectList.Free;
     end;
     // Delete the critical section
     DeleteCriticalSection(FLock);
  finally
     // Perform inherited
     inherited Destroy;
  end;

end;

//// TImportAddress ////////////////////////////////////////////////////////////
function TImportAddress.NextFunctionName(lpName: PChar): PChar;
begin

  // Skip to the next function name
  result:=StrEnd(lpName);

  // Check distance
  case Odd((result-lpName) mod 4) of
     // Push by 4
     False    :  Inc(result, 4);
     // Push by 3
     True     :  Inc(result, 3);
  end;

end;

function TImportAddress.ListContainsIAT(List: PIATListEntry; IAT: IndirectAddr): Boolean;
var  lpEntry:       PIATListEntry;
begin

  // Set default result
  result:=False;

  // Check list pointer
  if Assigned(List) then
  begin
     // Get list head
     lpEntry:=List;
     // Walk the list
     while Assigned(lpEntry) do
     begin
        // Check to see if the entry contains the IAT
        if (lpEntry^.IAT = IAT) then
        begin
           // Found the IAT in the list
           result:=True;
           // Done processing
           break;
        end;
        // Move to next entry
        lpEntry:=lpEntry^.Next;
     end;
  end;

end;

function TImportAddress.CompareLibraryNames(Name1, Name2: String): Boolean;
begin

  // Check names
  if (Length(Name1) > 0) and (Length(Name2) > 0) then
     // Extract the file names without extensions and compare with case
     result:=(CompareText(ChangeFileExt(ExtractFileName(Name1), EmptyStr), ChangeFileExt(ExtractFileName(Name2), EmptyStr)) = 0)
  else
     // One or both names are zero length
     result:=False;

end;

function TImportAddress.UpdateEntry(Entry: PIATListEntry; FunctionAddress: Pointer): Boolean;
var  dwProtect:     DWORD;
begin

  // Check IAT entry assignment
  if Assigned(Entry) then
  begin
     // Make sure we can modify the memory
     if VirtualProtect(Entry^.IAT, SizeOf(Pointer), PAGE_EXECUTE_READWRITE, dwProtect) then
     begin
        // Update the IAT entry with the function address
        Entry^.IAT^:=FunctionAddress;
        // Reset the memory protection
        VirtualProtect(Entry^.IAT, SizeOf(Pointer), dwProtect, dwProtect);
        // Flush the instruction cache
        FlushInstructionCache(GetCurrentProcess, nil, 0);
        // Success
        result:=True;
     end
     else
        // Failed to unprotect memory
        result:=False;
  end
  else
     // Failure
     result:=False;

end;

function TImportAddress.UpdatePatch(PatchRecord: PPatchRecord; FunctionAddress: Pointer): Boolean;
var  lpEntry:       PIATListEntry;
begin

  // Check patch record
  if Assigned(PatchRecord) then
  begin
     // Get first entry
     lpEntry:=PatchRecord^.IATList;
     // Walk the IAT entry list and update
     while Assigned(lpEntry) do
     begin
        // Update the entry
        UpdateEntry(lpEntry, FunctionAddress);
        // Get next entry
        lpEntry:=lpEntry^.Next;
     end;
     // Success
     result:=True;
  end
  else
     // Failure
     result:=False;

end;

procedure TImportAddress.EnumIATList(Module: HMODULE; LibraryName, FunctionName: String; var List: PIATListEntry);
var  lpNTHeaders:   PImageNtHeaders;
     lpDosHeader:   PImageDosHeader;
     lpImageDesc:   PImageImportDesc;
     lpHintTable:   PHintTable;
     lpEntry:       PIATListEntry;
     lpIAT:         IndirectAddr;
     lpszName:      PChar;
     dwEndDesc:     DWORD;
     dwLimit:       DWORD;
begin

  // Don't patch system modules on Win9x systems
  if (Module > 0) and (((GetVersion and $80000000) = 0) or (Module < $80000000)) then
  begin
     // Convert module pointer to DOS header
     lpDosHeader:=Pointer(Module);
     // Get the NT header
     lpNTHeaders:=Pointer(Integer(lpDosHeader) + lpDosHeader^._lfanew);
     // Get the max valid address size
     dwLimit:=DWORD(lpDosHeader) + lpNTHeaders^.OptionalHeader.SizeOfImage;
     // Handle the NT image imports
     with lpNTHeaders^.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_IMPORT] do
     begin
        // Get import entry description
        lpImageDesc:=Pointer(DWORD(lpDosHeader)+VirtualAddress);
        // Get ending point for description
        dwEndDesc:=lpImageDesc^.FirstThunk+Size;
     end;
     // Continue while the image description is still valid
     while (lpImageDesc^.Name > 0) and (lpImageDesc^.FirstThunk < dwEndDesc) and (lpImageDesc^.FirstThunk > 0) do
     begin
        // Get the libary name
        lpszName:=PChar(DWORD(lpDosHeader) + lpImageDesc^.Name);
        // Check against the passed library name
        if CompareLibraryNames(LibraryName, lpszName) then
        begin
           // Get the start of the import address table
           lpIAT:=Pointer(DWORD(lpDosHeader) + lpImageDesc^.FirstThunk);
           // Check the function name list
           if (lpImageDesc^.FuncNameList > 0) then
              // Used hint table
              lpHintTable:=Pointer(DWORD(lpDosHeader) + lpImageDesc^.FuncNameList)
           else
              // No hint table
              lpHintTable:=nil;
           // Iterate the IAT table
           while Assigned(lpIAT) and Assigned(lpIAT^)do
           begin
              // Check the function name hint table
              if Assigned(lpHintTable) then
              begin
                 // Check for valid name
                 if (DWORD(lpDosHeader) + lpHintTable^.FuncName + 2 > DWORD(lpDosHeader)) and
                    (DWORD(lpDosHeader) + lpHintTable^.FuncName + 2 < dwLimit) then
                    // Set name from hint
                    lpszName:=Pointer(DWORD(lpDosHeader) + lpHintTable^.FuncName + 2)
                 else
                    // Clear name
                    lpszName:=nil;
                 // Increment the name pointer table
                 Inc(lpHintTable);
              end
              else
                 // Get the function name
                 lpszName:=NextFunctionName(lpszName);
              // Check for assigned function name
              if Assigned(lpszName) then
              begin
                 // Check against passed name
                 if (StrIComp(PChar(FunctionName), lpszName) = 0) then
                 begin
                    // Check to see if this is already in the list
                    if not(ListContainsIAT(List, lpIAT)) then
                    begin
                       // Create new list entry
                       lpEntry:=AllocMem(SizeOf(TIATListEntry));
                       // Update the IAT entry address
                       lpEntry^.IAT:=lpIAT;
                       // Set next pointer
                       lpEntry^.Next:=List;
                       // Update the head of the list
                       List:=lpEntry;
                    end;
                 end;
              end;
              // Get the next import table entry
              Inc(lpIAT);
           end;
        end
        else
           // Enumerate the library
           EnumIATList(GetModuleHandle(lpszName), LibraryName, FunctionName, List);
        // Get the next import library description
        Inc(lpImageDesc);
     end;
  end;

end;

procedure TImportAddress.BuildIATPatchList(LibraryName, FunctionName: String; var List: PIATListEntry);
begin

  // Enum the IAT for the process
  EnumIATList(GetModuleHandle(nil), LibraryName, FunctionName, List);

  // If we are a library, then patch our IAT as well
  if IsLibrary then EnumIATList(HInstance, LibraryName, FunctionName, List);

end;

procedure TImportAddress.Unpatch(LibraryName, FunctionName: String);
var  lpPatch:       PPatchRecord;
     lpEntry:       PIATListEntry;
begin

  // Enter critical section
  EnterCriticalSection(FLock);

  // Resource protection
  try
     // Attempt to find the matching patch record
     lpPatch:=Find(LibraryName, FunctionName);
     // Did we find the patch record?
     if Assigned(lpPatch) then
     begin
        // Remove from patch list
        FPatchList.Remove(lpPatch);
        // Resource protection
        try
           // Walk the IAT entries and reset them to their old function address
           while Assigned(lpPatch^.IATList) do
           begin
              // Get current IAT entry item
              lpEntry:=lpPatch^.IATList;
              // Move list head to next entry
              lpPatch^.IATList:=lpPatch^.IATList^.Next;
              // Unpatch the entry
              UpdateEntry(lpEntry, lpPatch^.FunctionAddr);
              // Free the memory for the entry
              FreeMem(lpEntry);
           end;
        finally
           // Free the patch record memory
           FreeMem(lpPatch);
        end;
     end;
  finally
     // Leave critical section
     LeaveCriticalSection(FLock);
  end;

end;

function TImportAddress.Patch(LibraryName, FunctionName: String; NewFunction: Pointer; out OldFunction: Pointer): Boolean;
var  lpPatch:       PPatchRecord;
     lpFunction:    Pointer;
     hLibrary:      HMODULE;
begin

  // Enter critical section
  EnterCriticalSection(FLock);

  // Resource protection
  try
     // Clear old function pointer
     OldFunction:=nil;
     // Check parameters
     if (Length(LibraryName) > 0) and (Length(FunctionName) > 0) and Assigned(NewFunction) then
     begin
        // Determine if we already have a hook record for the library / function
        lpPatch:=Find(LibraryName, FunctionName);
        // Check patch record pointer
        if Assigned(lpPatch) then
        begin
           // Set old function pointer
           OldFunction:=lpPatch^.FunctionAddr;
           // Update the patch record with the new function pointer
           result:=UpdatePatch(lpPatch, NewFunction);
        end
        else
        begin
           // Attempt to get the module handle
           hLibrary:=GetModuleHandle(PChar(LibraryName));
           // Is the library loaded
           if (hLibrary = 0) then
              // Not loaded, so not statically bound, so we are not going to touch it
              result:=False
           else
           begin
              // Get the function address from the library. This address is returned from the
              // libraries EAT (export address table), and will not be modified by our IAT updates.
              lpFunction:=GetProcAddress(hLibrary, PChar(FunctionName));
              // Check function pointer
              if Assigned(lpFunction) then
              begin
                 // Allocate a new patch record
                 lpPatch:=AllocMem(SizeOf(TPatchRecord));
                 // Resource protection
                 try
                    // Set library name
                    StrPLCopy(@lpPatch^.LibraryName, ChangeFileExt(ExtractFileName(LibraryName), EmptyStr), MAX_LIBRARYNAME);
                    // Set the function name
                    StrPLCopy(@lpPatch^.FunctionName, FunctionName, MAX_FUNCTIONNAME);
                    // Set the function pointer
                    lpPatch^.FunctionAddr:=lpFunction;
                    // Set old function pointer
                    OldFunction:=lpPatch^.FunctionAddr;
                    // Build the IAT patch list
                    BuildIATPatchList(lpPatch^.LibraryName, FunctionName, lpPatch^.IATList);
                    // Update with the new function pointer
                    result:=UpdatePatch(lpPatch, NewFunction);
                 finally
                    // Add record to patch list
                    FPatchList.Add(lpPatch);
                 end;
              end
              else
                 // Failed to get the function address
                 result:=False;
           end;
        end;
     end
     else
        // Invalid parameters
        result:=False;
  finally
     // Leave critical section
     LeaveCriticalSection(FLock);
  end;

end;

function TImportAddress.Find(LibraryName, FunctionName: String): PPatchRecord;
var  dwIndex:       Integer;
begin

  // Enter critical section
  EnterCriticalSection(FLock);

  // Resource protection
  try
     // Set default result
     result:=nil;
     // Walk the patch record list
     for dwIndex:=0 to Pred(FPatchList.Count) do
     begin
        // Check library names
        if CompareLibraryNames(PPatchRecord(FPatchList[dwIndex])^.LibraryName, LibraryName) then
        begin
           // Check function names
           if (CompareText(PPatchRecord(FPatchList[dwIndex])^.FunctionName, FunctionName) = 0) then
           begin
              // Found the record
              result:=FPatchList[dwIndex];
              // Done processing
              break;
           end;
        end;
     end;
  finally
     // Leave the critical section
     LeaveCriticalSection(FLock);
  end;

end;

procedure TImportAddress.Delete(Index: Integer; Unpatch: Boolean);
var  lpPatch:       PPatchRecord;
     lpEntry:       PIATListEntry;
begin

  // Enter critical section
  EnterCriticalSection(FLock);

  // Resource protection
  try
     // Get the patch record at the given index
     lpPatch:=FPatchList[Index];
     // Resource protection
     try
        // Delete from the patch list
        FPatchList.Delete(Index);
        // Walk the IAT entries and reset them to their old function address
        while Assigned(lpPatch^.IATList) do
        begin
           // Get current IAT entry item
           lpEntry:=lpPatch^.IATList;
           // Move list head to next entry
           lpPatch^.IATList:=lpPatch^.IATList^.Next;
           // Unpatch the entry if required
           if Unpatch then UpdateEntry(lpEntry, lpPatch^.FunctionAddr);
           // Free the memory for the entry
           FreeMem(lpEntry);
        end;
     finally
        // Free the patch record memory
        FreeMem(lpPatch);
     end;
  finally
     // Leave critical section
     LeaveCriticalSection(FLock);
  end;

end;

procedure TImportAddress.Clear(Unpatch: Boolean);
var  lpPatch:       PPatchRecord;
     lpEntry:       PIATListEntry;
     dwIndex:       Integer;
begin

  // Enter critical section
  EnterCriticalSection(FLock);

  // Resource protection
  try
     // Resource protection
     try
        // Walk the patch list
        for dwIndex:=0 to Pred(FPatchList.Count) do
        begin
           // Get the patch record
           lpPatch:=FPatchList[dwIndex];
           // Resource protection
           try
              // Walk the IAT entries and reset them to their old function address
              while Assigned(lpPatch^.IATList) do
              begin
                 // Get current IAT entry item
                 lpEntry:=lpPatch^.IATList;
                 // Move list head to next entry
                 lpPatch^.IATList:=lpPatch^.IATList^.Next;
                 // Unpatch the entry if required
                 if Unpatch then UpdateEntry(lpEntry, lpPatch^.FunctionAddr);
                 // Free the memory for the entry
                 FreeMem(lpEntry);
              end;
           finally
              // Free patch record
              FreeMem(lpPatch);
           end;
        end;
     finally
        // Clear the patch list
        FPatchList.Clear;
     end;
  finally
     // Leave critical section
     LeaveCriticalSection(FLock);
  end;

end;

function TImportAddress.GetPatchRecord(Index: Integer): PPatchRecord;
begin

  // Enter critical section
  EnterCriticalSection(FLock);

  // Resource protection
  try
     // Return the patch record at the given index
     result:=FPatchList[Index];
  finally
     // Leave critical section
     LeaveCriticalSection(FLock);
  end;

end;

function TImportAddress.GetUnpatchOnFree: Boolean;
begin

  // Enter critical section
  EnterCriticalSection(FLock);

  // Resource protection
  try
     // Return the value
     result:=FUnpatch;
  finally
     // Leave critical section
     LeaveCriticalSection(FLock);
  end;

end;

procedure TImportAddress.SetUnpatchOnFree(Value: Boolean);
begin

  // Enter critical section
  EnterCriticalSection(FLock);

  // Resource protection
  try
     // Update the value
     FUnpatch:=Value;
  finally
     // Leave critical section
     LeaveCriticalSection(FLock);
  end;

end;

function TImportAddress.GetCount: Integer;
begin

  // Enter critical section
  EnterCriticalSection(FLock);

  // Resource protection
  try
     // Return the count of patch records
     result:=FPatchList.Count;
  finally
     // Leave critical section
     LeaveCriticalSection(FLock);
  end;

end;

constructor TImportAddress.Create;
begin

  // Perform inherited
  inherited Create;

  // Create critical section
  InitializeCriticalSection(FLock);

  // Create list for holding patch information
  FPatchList:=TList.Create;

  // Set default unpatch on free setting to false
  FUnpatch:=False;

end;

destructor TImportAddress.Destroy;
begin

  // Resource protection
  try
     // Resource protection
     try
        // Clear the patch list
        Clear(FUnpatch);
     finally
        // Free the patch list
        FPatchList.Free;
     end;
     // Delete the critical section
     DeleteCriticalSection(FLock);
  finally
     // Perform inherited
     inherited Destroy;
  end;

end;

end.
