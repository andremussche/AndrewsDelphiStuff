unit Unit1;

interface

uses
  JclWin32,
  Windows;

  procedure LoadFunctions;

  procedure DbgHelpCreateUserDump;
  procedure DbgHelpCreateUserDumpW;
  procedure EnumDirTree;
  procedure EnumDirTreeW;
  procedure EnumerateLoadedModules;
  procedure EnumerateLoadedModules64;
  procedure EnumerateLoadedModulesEx;
  procedure EnumerateLoadedModulesExW;
  procedure EnumerateLoadedModulesW64;
  procedure ExtensionApiVersion;
  procedure FindDebugInfoFile;
  procedure FindDebugInfoFileEx;
  procedure FindDebugInfoFileExW;
  procedure FindExecutableImage;
  procedure FindExecutableImageEx;
  procedure FindExecutableImageExW;
  procedure FindFileInPath;
  procedure FindFileInSearchPath;
  procedure GetTimestampForLoadedLibrary;
  procedure ImageDirectoryEntryToData;
  procedure ImageDirectoryEntryToDataEx;
  procedure ImageNtHeader;
  procedure ImageRvaToSection;
  procedure ImageRvaToVa;
  procedure ImagehlpApiVersion;
  procedure ImagehlpApiVersionEx;
  procedure MakeSureDirectoryPathExists;
  procedure MapDebugInformation;
  procedure MiniDumpReadDumpStream;
  procedure MiniDumpWriteDump;
  procedure SearchTreeForFile;
  procedure SearchTreeForFileW;
  procedure StackWalk;
  procedure StackWalk64;
  procedure SymAddSourceStream;
  procedure SymAddSourceStreamA;
  procedure SymAddSourceStreamW;
  procedure SymAddSymbol;
  procedure SymAddSymbolW;
  procedure SymCleanup;
  procedure SymDeleteSymbol;
  procedure SymDeleteSymbolW;
  procedure SymEnumLines;
  procedure SymEnumLinesW;
  procedure SymEnumProcesses;
  procedure SymEnumSourceFileTokens;
  procedure SymEnumSourceFiles;
  procedure SymEnumSourceFilesW;
  procedure SymEnumSourceLines;
  procedure SymEnumSourceLinesW;
  procedure SymEnumSym;
  procedure SymEnumSymbols;
  procedure SymEnumSymbolsForAddr;
  procedure SymEnumSymbolsForAddrW;
  procedure SymEnumSymbolsW;
  procedure SymEnumTypes;
  procedure SymEnumTypesByName;
  procedure SymEnumTypesByNameW;
  procedure SymEnumTypesW;
  procedure SymEnumerateModules;
  procedure SymEnumerateModules64;
  procedure SymEnumerateModulesW64;
  procedure SymEnumerateSymbols;
  procedure SymEnumerateSymbols64;
  procedure SymEnumerateSymbolsW;
  procedure SymEnumerateSymbolsW64;
  procedure SymFindDebugInfoFile;
  procedure SymFindDebugInfoFileW;
  procedure SymFindExecutableImage;
  procedure SymFindExecutableImageW;
  procedure SymFindFileInPath;
  procedure SymFindFileInPathW;

  //procedure SymFromAddr;
  function SymFromAddr(hProcess: THandle; dwAddr: DWORD64; pdwDisplacement: PDWORD64; var Symbol: JclWin32.TImagehlpSymbol_): Bool; stdcall;

  //procedure SymFromAddrW;
  //function SymFromAddrW(hProcess: THandle; dwAddr: DWORD; pdwDisplacement: PDWORD; var Symbol: JclWin32.TImagehlpSymbolW): Bool; stdcall;
  function SymFromAddrW(hProcess: THandle; dwAddr: DWORD64; pdwDisplacement: PDWORD64; var Symbol: JclWin32.TImagehlpSymbolW): Bool; stdcall;

  procedure SymFromIndex;
  procedure SymFromIndexW;
  procedure SymFromName;
  procedure SymFromNameW;
  procedure SymFromToken;
  procedure SymFromTokenW;
  procedure SymFunctionTableAccess;
  procedure SymFunctionTableAccess64;
  procedure SymGetFileLineOffsets64;
  procedure SymGetHomeDirectory;
  procedure SymGetHomeDirectoryW;
  procedure SymGetLineFromAddr;
  procedure SymGetLineFromAddr64;
  procedure SymGetLineFromAddrW64;
  procedure SymGetLineFromName;
  procedure SymGetLineFromName64;
  procedure SymGetLineFromNameW64;
  procedure SymGetLineNext;
  procedure SymGetLineNext64;
  procedure SymGetLineNextW64;
  procedure SymGetLinePrev;
  procedure SymGetLinePrev64;
  procedure SymGetLinePrevW64;
  procedure SymGetModuleBase;
  procedure SymGetModuleBase64;
  procedure SymGetModuleInfo;
  procedure SymGetModuleInfo64;
  procedure SymGetModuleInfoW;
  procedure SymGetModuleInfoW64;
  procedure SymGetOmapBlockBase;
  procedure SymGetOmaps;
  procedure SymGetOptions;
  procedure SymGetScope;
  procedure SymGetScopeW;
  procedure SymGetSearchPath;
  procedure SymGetSearchPathW;
  procedure SymGetSourceFile;
  procedure SymGetSourceFileFromToken;
  procedure SymGetSourceFileFromTokenW;
  procedure SymGetSourceFileToken;
  procedure SymGetSourceFileTokenW;
  procedure SymGetSourceFileW;
  procedure SymGetSourceVarFromToken;
  procedure SymGetSourceVarFromTokenW;

  //procedure SymGetSymFromAddr;
  function SymGetSymFromAddr(hProcess: THandle; dwAddr: DWORD; pdwDisplacement: PDWORD; var Symbol: JclWin32.TImagehlpSymbolA): Bool; stdcall;

  procedure SymGetSymFromAddr64;
  procedure SymGetSymFromName;
  procedure SymGetSymFromName64;
  procedure SymGetSymNext;
  procedure SymGetSymNext64;
  procedure SymGetSymPrev;
  procedure SymGetSymPrev64;
  procedure SymGetSymbolFile;
  procedure SymGetSymbolFileW;
  procedure SymGetTypeFromName;
  procedure SymGetTypeFromNameW;
  procedure SymGetTypeInfo;
  procedure SymGetTypeInfoEx;
  procedure SymGetUnwindInfo;
  procedure SymInitialize;
  procedure SymInitializeW;
  procedure SymLoadModule;
  procedure SymLoadModule64;
  procedure SymLoadModuleEx;
  procedure SymLoadModuleExW;
  procedure SymMatchFileName;
  procedure SymMatchFileNameW;
  procedure SymMatchString;
  procedure SymMatchStringA;
  procedure SymMatchStringW;
  procedure SymNext;
  procedure SymNextW;
  procedure SymPrev;
  procedure SymPrevW;
  procedure SymRefreshModuleList;
  procedure SymRegisterCallback;
  procedure SymRegisterCallback64;
  procedure SymRegisterCallbackW64;
  procedure SymRegisterFunctionEntryCallback;
  procedure SymRegisterFunctionEntryCallback64;
  procedure SymSearch;
  procedure SymSearchW;
  procedure SymSetContext;
  procedure SymSetHomeDirectory;
  procedure SymSetHomeDirectoryW;
  procedure SymSetOptions;
  procedure SymSetParentWindow;
  procedure SymSetScopeFromAddr;
  procedure SymSetScopeFromIndex;
  procedure SymSetSearchPath;
  procedure SymSetSearchPathW;
  procedure SymSrvDeltaName;
  procedure SymSrvDeltaNameW;
  procedure SymSrvGetFileIndexInfo;
  procedure SymSrvGetFileIndexInfoW;
  procedure SymSrvGetFileIndexString;
  procedure SymSrvGetFileIndexStringW;
  procedure SymSrvGetFileIndexes;
  procedure SymSrvGetFileIndexesW;
  procedure SymSrvGetSupplement;
  procedure SymSrvGetSupplementW;
  procedure SymSrvIsStore;
  procedure SymSrvIsStoreW;
  procedure SymSrvStoreFile;
  procedure SymSrvStoreFileW;
  procedure SymSrvStoreSupplement;
  procedure SymSrvStoreSupplementW;
  procedure SymUnDName;
  procedure SymUnDName64;
  procedure SymUnloadModule;
  procedure SymUnloadModule64;
  procedure UnDecorateSymbolName;
  procedure UnDecorateSymbolNameW;
  procedure UnmapDebugInformation;
  procedure WinDbgExtensionDllInit;
  procedure block;
  procedure chksym;
  procedure dbghelp_;
  procedure dh;
  procedure fptr;
  procedure homedir;
  procedure itoldyouso;
  procedure lmi;
  procedure lminfo;
  procedure omap;
  procedure srcfiles;
  procedure stack_force_ebp;
  procedure stackdbg;
  procedure sym;
  procedure symsrv;
  procedure vc7fpo;

implementation

uses JclDebug;

type
  TSymGetSymFromAddrAFunc = function (hProcess: THandle; dwAddr: DWORD;
    pdwDisplacement: PDWORD; var Symbol: JclWin32.TImagehlpSymbolA): Bool; stdcall;
  TSymGetSymFromAddrAFunc_ = function (hProcess: THandle; dwAddr: DWORD64;
    pdwDisplacement: PDWORD64; var Symbol: JclWin32.TImagehlpSymbol_): Bool; stdcall;
//  TSymGetSymFromAddrWFunc_ = function (hProcess: THandle; dwAddr: DWORD;
//    pdwDisplacement: PDWORD; var Symbol: JclWin32.TImagehlpSymbolW): Bool; stdcall;
  TSymGetSymFromAddrWFunc = function (hProcess: THandle; dwAddr: DWORD64; pdwDisplacement: PDWORD64; var Symbol: JclWin32.TImagehlpSymbolW): Bool; stdcall;

const
  SYMF_OMAP_GENERATED = $00000001;
  SYMF_OMAP_MODIFIED  = $00000002;

var
  pDbgHelpCreateUserDump: Pointer;
  pDbgHelpCreateUserDumpW: Pointer;
  pEnumDirTree: Pointer;
  pEnumDirTreeW: Pointer;
  pEnumerateLoadedModules: Pointer;
  pEnumerateLoadedModules64: Pointer;
  pEnumerateLoadedModulesEx: Pointer;
  pEnumerateLoadedModulesExW: Pointer;
  pEnumerateLoadedModulesW64: Pointer;
  pExtensionApiVersion: Pointer;
  pFindDebugInfoFile: Pointer;
  pFindDebugInfoFileEx: Pointer;
  pFindDebugInfoFileExW: Pointer;
  pFindExecutableImage: Pointer;
  pFindExecutableImageEx: Pointer;
  pFindExecutableImageExW: Pointer;
  pFindFileInPath: Pointer;
  pFindFileInSearchPath: Pointer;
  pGetTimestampForLoadedLibrary: Pointer;
  pImageDirectoryEntryToData: Pointer;
  pImageDirectoryEntryToDataEx: Pointer;
  pImageNtHeader: Pointer;
  pImageRvaToSection: Pointer;
  pImageRvaToVa: Pointer;
  pImagehlpApiVersion: Pointer;
  pImagehlpApiVersionEx: Pointer;
  pMakeSureDirectoryPathExists: Pointer;
  pMapDebugInformation: Pointer;
  pMiniDumpReadDumpStream: Pointer;
  pMiniDumpWriteDump: Pointer;
  pSearchTreeForFile: Pointer;
  pSearchTreeForFileW: Pointer;
  pStackWalk: Pointer;
  pStackWalk64: Pointer;
  pSymAddSourceStream: Pointer;
  pSymAddSourceStreamA: Pointer;
  pSymAddSourceStreamW: Pointer;
  pSymAddSymbol: Pointer;
  pSymAddSymbolW: Pointer;
  pSymCleanup: Pointer;
  pSymDeleteSymbol: Pointer;
  pSymDeleteSymbolW: Pointer;
  pSymEnumLines: Pointer;
  pSymEnumLinesW: Pointer;
  pSymEnumProcesses: Pointer;
  pSymEnumSourceFileTokens: Pointer;
  pSymEnumSourceFiles: Pointer;
  pSymEnumSourceFilesW: Pointer;
  pSymEnumSourceLines: Pointer;
  pSymEnumSourceLinesW: Pointer;
  pSymEnumSym: Pointer;
  pSymEnumSymbols: Pointer;
  pSymEnumSymbolsForAddr: Pointer;
  pSymEnumSymbolsForAddrW: Pointer;
  pSymEnumSymbolsW: Pointer;
  pSymEnumTypes: Pointer;
  pSymEnumTypesByName: Pointer;
  pSymEnumTypesByNameW: Pointer;
  pSymEnumTypesW: Pointer;
  pSymEnumerateModules: Pointer;
  pSymEnumerateModules64: Pointer;
  pSymEnumerateModulesW64: Pointer;
  pSymEnumerateSymbols: Pointer;
  pSymEnumerateSymbols64: Pointer;
  pSymEnumerateSymbolsW: Pointer;
  pSymEnumerateSymbolsW64: Pointer;
  pSymFindDebugInfoFile: Pointer;
  pSymFindDebugInfoFileW: Pointer;
  pSymFindExecutableImage: Pointer;
  pSymFindExecutableImageW: Pointer;
  pSymFindFileInPath: Pointer;
  pSymFindFileInPathW: Pointer;

  //pSymFromAddr: Pointer;
  pSymFromAddr: TSymGetSymFromAddrAFunc_;
  //pSymFromAddrW: Pointer;
  pSymFromAddrW: TSymGetSymFromAddrWFunc;

  pSymFromIndex: Pointer;
  pSymFromIndexW: Pointer;
  pSymFromName: Pointer;
  pSymFromNameW: Pointer;
  pSymFromToken: Pointer;
  pSymFromTokenW: Pointer;
  pSymFunctionTableAccess: Pointer;
  pSymFunctionTableAccess64: Pointer;
  pSymGetFileLineOffsets64: Pointer;
  pSymGetHomeDirectory: Pointer;
  pSymGetHomeDirectoryW: Pointer;
  pSymGetLineFromAddr: Pointer;
  pSymGetLineFromAddr64: Pointer;
  pSymGetLineFromAddrW64: Pointer;
  pSymGetLineFromName: Pointer;
  pSymGetLineFromName64: Pointer;
  pSymGetLineFromNameW64: Pointer;
  pSymGetLineNext: Pointer;
  pSymGetLineNext64: Pointer;
  pSymGetLineNextW64: Pointer;
  pSymGetLinePrev: Pointer;
  pSymGetLinePrev64: Pointer;
  pSymGetLinePrevW64: Pointer;
  pSymGetModuleBase: Pointer;
  pSymGetModuleBase64: Pointer;
  pSymGetModuleInfo: Pointer;
  pSymGetModuleInfo64: Pointer;
  pSymGetModuleInfoW: Pointer;
  pSymGetModuleInfoW64: Pointer;
  pSymGetOmapBlockBase: Pointer;
  pSymGetOmaps: Pointer;
  pSymGetOptions: Pointer;
  pSymGetScope: Pointer;
  pSymGetScopeW: Pointer;
  pSymGetSearchPath: Pointer;
  pSymGetSearchPathW: Pointer;
  pSymGetSourceFile: Pointer;
  pSymGetSourceFileFromToken: Pointer;
  pSymGetSourceFileFromTokenW: Pointer;
  pSymGetSourceFileToken: Pointer;
  pSymGetSourceFileTokenW: Pointer;
  pSymGetSourceFileW: Pointer;
  pSymGetSourceVarFromToken: Pointer;
  pSymGetSourceVarFromTokenW: Pointer;
  //pSymGetSymFromAddr: Pointer;
  pSymGetSymFromAddr: TSymGetSymFromAddrAFunc;
  pSymGetSymFromAddr64: Pointer;
  pSymGetSymFromName: Pointer;
  pSymGetSymFromName64: Pointer;
  pSymGetSymNext: Pointer;
  pSymGetSymNext64: Pointer;
  pSymGetSymPrev: Pointer;
  pSymGetSymPrev64: Pointer;
  pSymGetSymbolFile: Pointer;
  pSymGetSymbolFileW: Pointer;
  pSymGetTypeFromName: Pointer;
  pSymGetTypeFromNameW: Pointer;
  pSymGetTypeInfo: Pointer;
  pSymGetTypeInfoEx: Pointer;
  pSymGetUnwindInfo: Pointer;
  pSymInitialize: Pointer;
  pSymInitializeW: Pointer;
  pSymLoadModule: Pointer;
  pSymLoadModule64: Pointer;
  pSymLoadModuleEx: Pointer;
  pSymLoadModuleExW: Pointer;
  pSymMatchFileName: Pointer;
  pSymMatchFileNameW: Pointer;
  pSymMatchString: Pointer;
  pSymMatchStringA: Pointer;
  pSymMatchStringW: Pointer;
  pSymNext: Pointer;
  pSymNextW: Pointer;
  pSymPrev: Pointer;
  pSymPrevW: Pointer;
  pSymRefreshModuleList: Pointer;
  pSymRegisterCallback: Pointer;
  pSymRegisterCallback64: Pointer;
  pSymRegisterCallbackW64: Pointer;
  pSymRegisterFunctionEntryCallback: Pointer;
  pSymRegisterFunctionEntryCallback64: Pointer;
  pSymSearch: Pointer;
  pSymSearchW: Pointer;
  pSymSetContext: Pointer;
  pSymSetHomeDirectory: Pointer;
  pSymSetHomeDirectoryW: Pointer;
  pSymSetOptions: Pointer;
  pSymSetParentWindow: Pointer;
  pSymSetScopeFromAddr: Pointer;
  pSymSetScopeFromIndex: Pointer;
  pSymSetSearchPath: Pointer;
  pSymSetSearchPathW: Pointer;
  pSymSrvDeltaName: Pointer;
  pSymSrvDeltaNameW: Pointer;
  pSymSrvGetFileIndexInfo: Pointer;
  pSymSrvGetFileIndexInfoW: Pointer;
  pSymSrvGetFileIndexString: Pointer;
  pSymSrvGetFileIndexStringW: Pointer;
  pSymSrvGetFileIndexes: Pointer;
  pSymSrvGetFileIndexesW: Pointer;
  pSymSrvGetSupplement: Pointer;
  pSymSrvGetSupplementW: Pointer;
  pSymSrvIsStore: Pointer;
  pSymSrvIsStoreW: Pointer;
  pSymSrvStoreFile: Pointer;
  pSymSrvStoreFileW: Pointer;
  pSymSrvStoreSupplement: Pointer;
  pSymSrvStoreSupplementW: Pointer;
  pSymUnDName: Pointer;
  pSymUnDName64: Pointer;
  pSymUnloadModule: Pointer;
  pSymUnloadModule64: Pointer;
  pUnDecorateSymbolName: Pointer;
  pUnDecorateSymbolNameW: Pointer;
  pUnmapDebugInformation: Pointer;
  pWinDbgExtensionDllInit: Pointer;
  pblock: Pointer;
  pchksym: Pointer;
  pdbghelp: Pointer;
  pdh: Pointer;
  pfptr: Pointer;
  phomedir: Pointer;
  pitoldyouso: Pointer;
  plmi: Pointer;
  plminfo: Pointer;
  pomap: Pointer;
  psrcfiles: Pointer;
  pstack_force_ebp: Pointer;
  pstackdbg: Pointer;
  psym: Pointer;
  psymsrv: Pointer;
  pvc7fpo: Pointer;

var
  hDLL: THandle;

procedure LoadFunctions;
begin
  hDLL := LoadLibrary('dbghelp.dll.org');

  pDbgHelpCreateUserDump := GetProcAddress(hDll, 'DbgHelpCreateUserDump');
  pDbgHelpCreateUserDumpW := GetProcAddress(hDll, 'DbgHelpCreateUserDumpW');
  pEnumDirTree := GetProcAddress(hDll, 'EnumDirTree');
  pEnumDirTreeW := GetProcAddress(hDll, 'EnumDirTreeW');
  pEnumerateLoadedModules := GetProcAddress(hDll, 'EnumerateLoadedModules');
  pEnumerateLoadedModules64 := GetProcAddress(hDll, 'EnumerateLoadedModules64');
  pEnumerateLoadedModulesEx := GetProcAddress(hDll, 'EnumerateLoadedModulesEx');
  pEnumerateLoadedModulesExW := GetProcAddress(hDll, 'EnumerateLoadedModulesExW');
  pEnumerateLoadedModulesW64 := GetProcAddress(hDll, 'EnumerateLoadedModulesW64');
  pExtensionApiVersion := GetProcAddress(hDll, 'ExtensionApiVersion');
  pFindDebugInfoFile := GetProcAddress(hDll, 'FindDebugInfoFile');
  pFindDebugInfoFileEx := GetProcAddress(hDll, 'FindDebugInfoFileEx');
  pFindDebugInfoFileExW := GetProcAddress(hDll, 'FindDebugInfoFileExW');
  pFindExecutableImage := GetProcAddress(hDll, 'FindExecutableImage');
  pFindExecutableImageEx := GetProcAddress(hDll, 'FindExecutableImageEx');
  pFindExecutableImageExW := GetProcAddress(hDll, 'FindExecutableImageExW');
  pFindFileInPath := GetProcAddress(hDll, 'FindFileInPath');
  pFindFileInSearchPath := GetProcAddress(hDll, 'FindFileInSearchPath');
  pGetTimestampForLoadedLibrary := GetProcAddress(hDll, 'GetTimestampForLoadedLibrary');
  pImageDirectoryEntryToData := GetProcAddress(hDll, 'ImageDirectoryEntryToData');
  pImageDirectoryEntryToDataEx := GetProcAddress(hDll, 'ImageDirectoryEntryToDataEx');
  pImageNtHeader := GetProcAddress(hDll, 'ImageNtHeader');
  pImageRvaToSection := GetProcAddress(hDll, 'ImageRvaToSection');
  pImageRvaToVa := GetProcAddress(hDll, 'ImageRvaToVa');
  pImagehlpApiVersion := GetProcAddress(hDll, 'ImagehlpApiVersion');
  pImagehlpApiVersionEx := GetProcAddress(hDll, 'ImagehlpApiVersionEx');
  pMakeSureDirectoryPathExists := GetProcAddress(hDll, 'MakeSureDirectoryPathExists');
  pMapDebugInformation := GetProcAddress(hDll, 'MapDebugInformation');
  pMiniDumpReadDumpStream := GetProcAddress(hDll, 'MiniDumpReadDumpStream');
  pMiniDumpWriteDump := GetProcAddress(hDll, 'MiniDumpWriteDump');
  pSearchTreeForFile := GetProcAddress(hDll, 'SearchTreeForFile');
  pSearchTreeForFileW := GetProcAddress(hDll, 'SearchTreeForFileW');
  pStackWalk := GetProcAddress(hDll, 'StackWalk');
  pStackWalk64 := GetProcAddress(hDll, 'StackWalk64');
  pSymAddSourceStream := GetProcAddress(hDll, 'SymAddSourceStream');
  pSymAddSourceStreamA := GetProcAddress(hDll, 'SymAddSourceStreamA');
  pSymAddSourceStreamW := GetProcAddress(hDll, 'SymAddSourceStreamW');
  pSymAddSymbol := GetProcAddress(hDll, 'SymAddSymbol');
  pSymAddSymbolW := GetProcAddress(hDll, 'SymAddSymbolW');
  pSymCleanup := GetProcAddress(hDll, 'SymCleanup');
  pSymDeleteSymbol := GetProcAddress(hDll, 'SymDeleteSymbol');
  pSymDeleteSymbolW := GetProcAddress(hDll, 'SymDeleteSymbolW');
  pSymEnumLines := GetProcAddress(hDll, 'SymEnumLines');
  pSymEnumLinesW := GetProcAddress(hDll, 'SymEnumLinesW');
  pSymEnumProcesses := GetProcAddress(hDll, 'SymEnumProcesses');
  pSymEnumSourceFileTokens := GetProcAddress(hDll, 'SymEnumSourceFileTokens');
  pSymEnumSourceFiles := GetProcAddress(hDll, 'SymEnumSourceFiles');
  pSymEnumSourceFilesW := GetProcAddress(hDll, 'SymEnumSourceFilesW');
  pSymEnumSourceLines := GetProcAddress(hDll, 'SymEnumSourceLines');
  pSymEnumSourceLinesW := GetProcAddress(hDll, 'SymEnumSourceLinesW');
  pSymEnumSym := GetProcAddress(hDll, 'SymEnumSym');
  pSymEnumSymbols := GetProcAddress(hDll, 'SymEnumSymbols');
  pSymEnumSymbolsForAddr := GetProcAddress(hDll, 'SymEnumSymbolsForAddr');
  pSymEnumSymbolsForAddrW := GetProcAddress(hDll, 'SymEnumSymbolsForAddrW');
  pSymEnumSymbolsW := GetProcAddress(hDll, 'SymEnumSymbolsW');
  pSymEnumTypes := GetProcAddress(hDll, 'SymEnumTypes');
  pSymEnumTypesByName := GetProcAddress(hDll, 'SymEnumTypesByName');
  pSymEnumTypesByNameW := GetProcAddress(hDll, 'SymEnumTypesByNameW');
  pSymEnumTypesW := GetProcAddress(hDll, 'SymEnumTypesW');
  pSymEnumerateModules := GetProcAddress(hDll, 'SymEnumerateModules');
  pSymEnumerateModules64 := GetProcAddress(hDll, 'SymEnumerateModules64');
  pSymEnumerateModulesW64 := GetProcAddress(hDll, 'SymEnumerateModulesW64');
  pSymEnumerateSymbols := GetProcAddress(hDll, 'SymEnumerateSymbols');
  pSymEnumerateSymbols64 := GetProcAddress(hDll, 'SymEnumerateSymbols64');
  pSymEnumerateSymbolsW := GetProcAddress(hDll, 'SymEnumerateSymbolsW');
  pSymEnumerateSymbolsW64 := GetProcAddress(hDll, 'SymEnumerateSymbolsW64');
  pSymFindDebugInfoFile := GetProcAddress(hDll, 'SymFindDebugInfoFile');
  pSymFindDebugInfoFileW := GetProcAddress(hDll, 'SymFindDebugInfoFileW');
  pSymFindExecutableImage := GetProcAddress(hDll, 'SymFindExecutableImage');
  pSymFindExecutableImageW := GetProcAddress(hDll, 'SymFindExecutableImageW');
  pSymFindFileInPath := GetProcAddress(hDll, 'SymFindFileInPath');
  pSymFindFileInPathW := GetProcAddress(hDll, 'SymFindFileInPathW');
  pSymFromAddr := GetProcAddress(hDll, 'SymFromAddr');
  pSymFromAddrW := GetProcAddress(hDll, 'SymFromAddrW');
  pSymFromIndex := GetProcAddress(hDll, 'SymFromIndex');
  pSymFromIndexW := GetProcAddress(hDll, 'SymFromIndexW');
  pSymFromName := GetProcAddress(hDll, 'SymFromName');
  pSymFromNameW := GetProcAddress(hDll, 'SymFromNameW');
  pSymFromToken := GetProcAddress(hDll, 'SymFromToken');
  pSymFromTokenW := GetProcAddress(hDll, 'SymFromTokenW');
  pSymFunctionTableAccess := GetProcAddress(hDll, 'SymFunctionTableAccess');
  pSymFunctionTableAccess64 := GetProcAddress(hDll, 'SymFunctionTableAccess64');
  pSymGetFileLineOffsets64 := GetProcAddress(hDll, 'SymGetFileLineOffsets64');
  pSymGetHomeDirectory := GetProcAddress(hDll, 'SymGetHomeDirectory');
  pSymGetHomeDirectoryW := GetProcAddress(hDll, 'SymGetHomeDirectoryW');
  pSymGetLineFromAddr := GetProcAddress(hDll, 'SymGetLineFromAddr');
  pSymGetLineFromAddr64 := GetProcAddress(hDll, 'SymGetLineFromAddr64');
  pSymGetLineFromAddrW64 := GetProcAddress(hDll, 'SymGetLineFromAddrW64');
  pSymGetLineFromName := GetProcAddress(hDll, 'SymGetLineFromName');
  pSymGetLineFromName64 := GetProcAddress(hDll, 'SymGetLineFromName64');
  pSymGetLineFromNameW64 := GetProcAddress(hDll, 'SymGetLineFromNameW64');
  pSymGetLineNext := GetProcAddress(hDll, 'SymGetLineNext');
  pSymGetLineNext64 := GetProcAddress(hDll, 'SymGetLineNext64');
  pSymGetLineNextW64 := GetProcAddress(hDll, 'SymGetLineNextW64');
  pSymGetLinePrev := GetProcAddress(hDll, 'SymGetLinePrev');
  pSymGetLinePrev64 := GetProcAddress(hDll, 'SymGetLinePrev64');
  pSymGetLinePrevW64 := GetProcAddress(hDll, 'SymGetLinePrevW64');
  pSymGetModuleBase := GetProcAddress(hDll, 'SymGetModuleBase');
  pSymGetModuleBase64 := GetProcAddress(hDll, 'SymGetModuleBase64');
  pSymGetModuleInfo := GetProcAddress(hDll, 'SymGetModuleInfo');
  pSymGetModuleInfo64 := GetProcAddress(hDll, 'SymGetModuleInfo64');
  pSymGetModuleInfoW := GetProcAddress(hDll, 'SymGetModuleInfoW');
  pSymGetModuleInfoW64 := GetProcAddress(hDll, 'SymGetModuleInfoW64');
  pSymGetOmapBlockBase := GetProcAddress(hDll, 'SymGetOmapBlockBase');
  pSymGetOmaps := GetProcAddress(hDll, 'SymGetOmaps');
  pSymGetOptions := GetProcAddress(hDll, 'SymGetOptions');
  pSymGetScope := GetProcAddress(hDll, 'SymGetScope');
  pSymGetScopeW := GetProcAddress(hDll, 'SymGetScopeW');
  pSymGetSearchPath := GetProcAddress(hDll, 'SymGetSearchPath');
  pSymGetSearchPathW := GetProcAddress(hDll, 'SymGetSearchPathW');
  pSymGetSourceFile := GetProcAddress(hDll, 'SymGetSourceFile');
  pSymGetSourceFileFromToken := GetProcAddress(hDll, 'SymGetSourceFileFromToken');
  pSymGetSourceFileFromTokenW := GetProcAddress(hDll, 'SymGetSourceFileFromTokenW');
  pSymGetSourceFileToken := GetProcAddress(hDll, 'SymGetSourceFileToken');
  pSymGetSourceFileTokenW := GetProcAddress(hDll, 'SymGetSourceFileTokenW');
  pSymGetSourceFileW := GetProcAddress(hDll, 'SymGetSourceFileW');
  pSymGetSourceVarFromToken := GetProcAddress(hDll, 'SymGetSourceVarFromToken');
  pSymGetSourceVarFromTokenW := GetProcAddress(hDll, 'SymGetSourceVarFromTokenW');
  pSymGetSymFromAddr := GetProcAddress(hDll, 'SymGetSymFromAddr');
  pSymGetSymFromAddr64 := GetProcAddress(hDll, 'SymGetSymFromAddr64');
  pSymGetSymFromName := GetProcAddress(hDll, 'SymGetSymFromName');
  pSymGetSymFromName64 := GetProcAddress(hDll, 'SymGetSymFromName64');
  pSymGetSymNext := GetProcAddress(hDll, 'SymGetSymNext');
  pSymGetSymNext64 := GetProcAddress(hDll, 'SymGetSymNext64');
  pSymGetSymPrev := GetProcAddress(hDll, 'SymGetSymPrev');
  pSymGetSymPrev64 := GetProcAddress(hDll, 'SymGetSymPrev64');
  pSymGetSymbolFile := GetProcAddress(hDll, 'SymGetSymbolFile');
  pSymGetSymbolFileW := GetProcAddress(hDll, 'SymGetSymbolFileW');
  pSymGetTypeFromName := GetProcAddress(hDll, 'SymGetTypeFromName');
  pSymGetTypeFromNameW := GetProcAddress(hDll, 'SymGetTypeFromNameW');
  pSymGetTypeInfo := GetProcAddress(hDll, 'SymGetTypeInfo');
  pSymGetTypeInfoEx := GetProcAddress(hDll, 'SymGetTypeInfoEx');
  pSymGetUnwindInfo := GetProcAddress(hDll, 'SymGetUnwindInfo');
  pSymInitialize := GetProcAddress(hDll, 'SymInitialize');
  pSymInitializeW := GetProcAddress(hDll, 'SymInitializeW');
  pSymLoadModule := GetProcAddress(hDll, 'SymLoadModule');
  pSymLoadModule64 := GetProcAddress(hDll, 'SymLoadModule64');
  pSymLoadModuleEx := GetProcAddress(hDll, 'SymLoadModuleEx');
  pSymLoadModuleExW := GetProcAddress(hDll, 'SymLoadModuleExW');
  pSymMatchFileName := GetProcAddress(hDll, 'SymMatchFileName');
  pSymMatchFileNameW := GetProcAddress(hDll, 'SymMatchFileNameW');
  pSymMatchString := GetProcAddress(hDll, 'SymMatchString');
  pSymMatchStringA := GetProcAddress(hDll, 'SymMatchStringA');
  pSymMatchStringW := GetProcAddress(hDll, 'SymMatchStringW');
  pSymNext := GetProcAddress(hDll, 'SymNext');
  pSymNextW := GetProcAddress(hDll, 'SymNextW');
  pSymPrev := GetProcAddress(hDll, 'SymPrev');
  pSymPrevW := GetProcAddress(hDll, 'SymPrevW');
  pSymRefreshModuleList := GetProcAddress(hDll, 'SymRefreshModuleList');
  pSymRegisterCallback := GetProcAddress(hDll, 'SymRegisterCallback');
  pSymRegisterCallback64 := GetProcAddress(hDll, 'SymRegisterCallback64');
  pSymRegisterCallbackW64 := GetProcAddress(hDll, 'SymRegisterCallbackW64');
  pSymRegisterFunctionEntryCallback := GetProcAddress(hDll, 'SymRegisterFunctionEntryCallback');
  pSymRegisterFunctionEntryCallback64 := GetProcAddress(hDll, 'SymRegisterFunctionEntryCallback64');
  pSymSearch := GetProcAddress(hDll, 'SymSearch');
  pSymSearchW := GetProcAddress(hDll, 'SymSearchW');
  pSymSetContext := GetProcAddress(hDll, 'SymSetContext');
  pSymSetHomeDirectory := GetProcAddress(hDll, 'SymSetHomeDirectory');
  pSymSetHomeDirectoryW := GetProcAddress(hDll, 'SymSetHomeDirectoryW');
  pSymSetOptions := GetProcAddress(hDll, 'SymSetOptions');
  pSymSetParentWindow := GetProcAddress(hDll, 'SymSetParentWindow');
  pSymSetScopeFromAddr := GetProcAddress(hDll, 'SymSetScopeFromAddr');
  pSymSetScopeFromIndex := GetProcAddress(hDll, 'SymSetScopeFromIndex');
  pSymSetSearchPath := GetProcAddress(hDll, 'SymSetSearchPath');
  pSymSetSearchPathW := GetProcAddress(hDll, 'SymSetSearchPathW');
  pSymSrvDeltaName := GetProcAddress(hDll, 'SymSrvDeltaName');
  pSymSrvDeltaNameW := GetProcAddress(hDll, 'SymSrvDeltaNameW');
  pSymSrvGetFileIndexInfo := GetProcAddress(hDll, 'SymSrvGetFileIndexInfo');
  pSymSrvGetFileIndexInfoW := GetProcAddress(hDll, 'SymSrvGetFileIndexInfoW');
  pSymSrvGetFileIndexString := GetProcAddress(hDll, 'SymSrvGetFileIndexString');
  pSymSrvGetFileIndexStringW := GetProcAddress(hDll, 'SymSrvGetFileIndexStringW');
  pSymSrvGetFileIndexes := GetProcAddress(hDll, 'SymSrvGetFileIndexes');
  pSymSrvGetFileIndexesW := GetProcAddress(hDll, 'SymSrvGetFileIndexesW');
  pSymSrvGetSupplement := GetProcAddress(hDll, 'SymSrvGetSupplement');
  pSymSrvGetSupplementW := GetProcAddress(hDll, 'SymSrvGetSupplementW');
  pSymSrvIsStore := GetProcAddress(hDll, 'SymSrvIsStore');
  pSymSrvIsStoreW := GetProcAddress(hDll, 'SymSrvIsStoreW');
  pSymSrvStoreFile := GetProcAddress(hDll, 'SymSrvStoreFile');
  pSymSrvStoreFileW := GetProcAddress(hDll, 'SymSrvStoreFileW');
  pSymSrvStoreSupplement := GetProcAddress(hDll, 'SymSrvStoreSupplement');
  pSymSrvStoreSupplementW := GetProcAddress(hDll, 'SymSrvStoreSupplementW');
  pSymUnDName := GetProcAddress(hDll, 'SymUnDName');
  pSymUnDName64 := GetProcAddress(hDll, 'SymUnDName64');
  pSymUnloadModule := GetProcAddress(hDll, 'SymUnloadModule');
  pSymUnloadModule64 := GetProcAddress(hDll, 'SymUnloadModule64');
  pUnDecorateSymbolName := GetProcAddress(hDll, 'UnDecorateSymbolName');
  pUnDecorateSymbolNameW := GetProcAddress(hDll, 'UnDecorateSymbolNameW');
  pUnmapDebugInformation := GetProcAddress(hDll, 'UnmapDebugInformation');
  pWinDbgExtensionDllInit := GetProcAddress(hDll, 'WinDbgExtensionDllInit');
  pblock := GetProcAddress(hDll, 'block');
  pchksym := GetProcAddress(hDll, 'chksym');
  pdbghelp := GetProcAddress(hDll, 'dbghelp');
  pdh := GetProcAddress(hDll, 'dh');
  pfptr := GetProcAddress(hDll, 'fptr');
  phomedir := GetProcAddress(hDll, 'homedir');
  pitoldyouso := GetProcAddress(hDll, 'itoldyouso');
  plmi := GetProcAddress(hDll, 'lmi');
  plminfo := GetProcAddress(hDll, 'lminfo');
  pomap := GetProcAddress(hDll, 'omap');
  psrcfiles := GetProcAddress(hDll, 'srcfiles');
  pstack_force_ebp := GetProcAddress(hDll, 'stack_force_ebp');
  pstackdbg := GetProcAddress(hDll, 'stackdbg');
  psym := GetProcAddress(hDll, 'sym');
  psymsrv := GetProcAddress(hDll, 'symsrv');
  pvc7fpo := GetProcAddress(hDll, 'vc7fpo');
end;

procedure DbgHelpCreateUserDump;
asm
  jmp pDbgHelpCreateUserDump;
end;

procedure DbgHelpCreateUserDumpW;
asm
  jmp pDbgHelpCreateUserDumpW;
end;

procedure EnumDirTree;
asm
  jmp pEnumDirTree;
end;

procedure EnumDirTreeW;
asm
  jmp pEnumDirTreeW;
end;

procedure EnumerateLoadedModules;
asm
  jmp pEnumerateLoadedModules;
end;

procedure EnumerateLoadedModules64;
asm
  jmp pEnumerateLoadedModules64;
end;

procedure EnumerateLoadedModulesEx;
asm
  jmp pEnumerateLoadedModulesEx;
end;

procedure EnumerateLoadedModulesExW;
asm
  jmp pEnumerateLoadedModulesExW;
end;

procedure EnumerateLoadedModulesW64;
asm
  jmp pEnumerateLoadedModulesW64;
end;

procedure ExtensionApiVersion;
asm
  jmp pExtensionApiVersion;
end;

procedure FindDebugInfoFile;
asm
  jmp pFindDebugInfoFile;
end;

procedure FindDebugInfoFileEx;
asm
  jmp pFindDebugInfoFileEx;
end;

procedure FindDebugInfoFileExW;
asm
  jmp pFindDebugInfoFileExW;
end;

procedure FindExecutableImage;
asm
  jmp pFindExecutableImage;
end;

procedure FindExecutableImageEx;
asm
  jmp pFindExecutableImageEx;
end;

procedure FindExecutableImageExW;
asm
  jmp pFindExecutableImageExW;
end;

procedure FindFileInPath;
asm
  jmp pFindFileInPath;
end;

procedure FindFileInSearchPath;
asm
  jmp pFindFileInSearchPath;
end;

procedure GetTimestampForLoadedLibrary;
asm
  jmp pGetTimestampForLoadedLibrary;
end;

procedure ImageDirectoryEntryToData;
asm
  jmp pImageDirectoryEntryToData;
end;

procedure ImageDirectoryEntryToDataEx;
asm
  jmp pImageDirectoryEntryToDataEx;
end;

procedure ImageNtHeader;
asm
  jmp pImageNtHeader;
end;

procedure ImageRvaToSection;
asm
  jmp pImageRvaToSection;
end;

procedure ImageRvaToVa;
asm
  jmp pImageRvaToVa;
end;

procedure ImagehlpApiVersion;
asm
  jmp pImagehlpApiVersion;
end;

procedure ImagehlpApiVersionEx;
asm
  jmp pImagehlpApiVersionEx;
end;

procedure MakeSureDirectoryPathExists;
asm
  jmp pMakeSureDirectoryPathExists;
end;

procedure MapDebugInformation;
asm
  jmp pMapDebugInformation;
end;

procedure MiniDumpReadDumpStream;
asm
  jmp pMiniDumpReadDumpStream;
end;

procedure MiniDumpWriteDump;
asm
  jmp pMiniDumpWriteDump;
end;

procedure SearchTreeForFile;
asm
  jmp pSearchTreeForFile;
end;

procedure SearchTreeForFileW;
asm
  jmp pSearchTreeForFileW;
end;

procedure StackWalk;
asm
  jmp pStackWalk;
end;

procedure StackWalk64;
asm
  jmp pStackWalk64;
end;

procedure SymAddSourceStream;
asm
  jmp pSymAddSourceStream;
end;

procedure SymAddSourceStreamA;
asm
  jmp pSymAddSourceStreamA;
end;

procedure SymAddSourceStreamW;
asm
  jmp pSymAddSourceStreamW;
end;

procedure SymAddSymbol;
asm
  jmp pSymAddSymbol;
end;

procedure SymAddSymbolW;
asm
  jmp pSymAddSymbolW;
end;

procedure SymCleanup;
asm
  jmp pSymCleanup;
end;

procedure SymDeleteSymbol;
asm
  jmp pSymDeleteSymbol;
end;

procedure SymDeleteSymbolW;
asm
  jmp pSymDeleteSymbolW;
end;

procedure SymEnumLines;
asm
  jmp pSymEnumLines;
end;

procedure SymEnumLinesW;
asm
  jmp pSymEnumLinesW;
end;

procedure SymEnumProcesses;
asm
  jmp pSymEnumProcesses;
end;

procedure SymEnumSourceFileTokens;
asm
  jmp pSymEnumSourceFileTokens;
end;

procedure SymEnumSourceFiles;
asm
  jmp pSymEnumSourceFiles;
end;

procedure SymEnumSourceFilesW;
asm
  jmp pSymEnumSourceFilesW;
end;

procedure SymEnumSourceLines;
asm
  jmp pSymEnumSourceLines;
end;

procedure SymEnumSourceLinesW;
asm
  jmp pSymEnumSourceLinesW;
end;

procedure SymEnumSym;
asm
  jmp pSymEnumSym;
end;

procedure SymEnumSymbols;
asm
  jmp pSymEnumSymbols;
end;

procedure SymEnumSymbolsForAddr;
asm
  jmp pSymEnumSymbolsForAddr;
end;

procedure SymEnumSymbolsForAddrW;
asm
  jmp pSymEnumSymbolsForAddrW;
end;

procedure SymEnumSymbolsW;
asm
  jmp pSymEnumSymbolsW;
end;

procedure SymEnumTypes;
asm
  jmp pSymEnumTypes;
end;

procedure SymEnumTypesByName;
asm
  jmp pSymEnumTypesByName;
end;

procedure SymEnumTypesByNameW;
asm
  jmp pSymEnumTypesByNameW;
end;

procedure SymEnumTypesW;
asm
  jmp pSymEnumTypesW;
end;

procedure SymEnumerateModules;
asm
  jmp pSymEnumerateModules;
end;

procedure SymEnumerateModules64;
asm
  jmp pSymEnumerateModules64;
end;

procedure SymEnumerateModulesW64;
asm
  jmp pSymEnumerateModulesW64;
end;

procedure SymEnumerateSymbols;
asm
  jmp pSymEnumerateSymbols;
end;

procedure SymEnumerateSymbols64;
asm
  jmp pSymEnumerateSymbols64;
end;

procedure SymEnumerateSymbolsW;
asm
  jmp pSymEnumerateSymbolsW;
end;

procedure SymEnumerateSymbolsW64;
asm
  jmp pSymEnumerateSymbolsW64;
end;

procedure SymFindDebugInfoFile;
asm
  jmp pSymFindDebugInfoFile;
end;

procedure SymFindDebugInfoFileW;
asm
  jmp pSymFindDebugInfoFileW;
end;

procedure SymFindExecutableImage;
asm
  jmp pSymFindExecutableImage;
end;

procedure SymFindExecutableImageW;
asm
  jmp pSymFindExecutableImageW;
end;

procedure SymFindFileInPath;
asm
  jmp pSymFindFileInPath;
end;

procedure SymFindFileInPathW;
asm
  jmp pSymFindFileInPathW;
end;

//procedure SymFromAddr;
//asm
//  jmp pSymFromAddr;
//end;

function SymFromAddr(hProcess: THandle; dwAddr: DWORD64; pdwDisplacement: PDWORD64; var Symbol: JclWin32.TImagehlpSymbol_): Bool; stdcall;
var
  s: ansistring;
  info: TJclLocationInfo;
begin
  Result := pSymFromAddr(hProcess, dwAddr, pdwDisplacement, Symbol);

  if not Result then
  begin
    //note: defined DEBUG_NO_SYMBOLS otherwise infite loop?

    jclDebug.SetRemoteProcess(hProcess);
    if jclDebug.GetLocationInfo(Pointer(dwAddr), info) then
    begin
      Result           := True;
      Symbol.Address   := dwAddr;
      //s := 'test' + #0;
      s := info.ProcedureName + #0;
      Move(s[1], Symbol.Name[0], Length(s));  //todo: check MaxNameLen!
      Symbol.NameLen := Length(s);
    end;
  end;

//  if Result then
//    s := PAnsiChar(@Symbol.Name[0]);
end;

//procedure SymFromAddrW;
//asm
//  jmp pSymFromAddrW;
//end;

function SymFromAddrW(hProcess: THandle; dwAddr: DWORD64; pdwDisplacement: PDWORD64; var Symbol: JclWin32.TImagehlpSymbolW): Bool; stdcall;
var
  s: string;
  info: TJclLocationInfo;
begin
  Result := pSymFromAddrW(hProcess, dwAddr, pdwDisplacement, Symbol);
  if not Result then
  begin
{
Name		Value
Symbol		(88, 0, 0, 0, 0, 0, 629, 880, 0, 2199023255552, 0, 0, 18446744071567308704, 0, 0, 10, 12, 127, ('K'))
	SizeOfStruct	88
	TypeIndex	0
	Reserved1	0
	Reserved2	0
	Reserved3	0
	Reserved4	0
	Reserved5	629
	Index	880
	Size	0
	ModBase	2199023255552
	Flags	0
	Value	0
	Address	18446744071567308704
	Register_	0
	Scope	0
	Tag	10
	NameLen	12
	MaxNameLen	127
	Name	('K')
}
    //note: defined DEBUG_NO_SYMBOLS otherwise infite loop?

    jclDebug.SetRemoteProcess(hProcess);
    if jclDebug.GetLocationInfo(Pointer(dwAddr), info) then
    begin
      Result           := True;
      Symbol.Address   := dwAddr;
      //s := 'test' + #0;
      s := info.ProcedureName + #0;
      Move(s[1], Symbol.Name[0], Length(s) * 2);    //todo: check MaxNameLen!
      Symbol.NameLen := Length(s) * 2;
    end;
  end;

//  if Result then
//    s := PChar(@Symbol.Name[0]);
end;

procedure SymFromIndex;
asm
  jmp pSymFromIndex;
end;

procedure SymFromIndexW;
asm
  jmp pSymFromIndexW;
end;

procedure SymFromName;
asm
  jmp pSymFromName;
end;

procedure SymFromNameW;
asm
  jmp pSymFromNameW;
end;

procedure SymFromToken;
asm
  jmp pSymFromToken;
end;

procedure SymFromTokenW;
asm
  jmp pSymFromTokenW;
end;

procedure SymFunctionTableAccess;
asm
  jmp pSymFunctionTableAccess;
end;

procedure SymFunctionTableAccess64;
asm
  jmp pSymFunctionTableAccess64;
end;

procedure SymGetFileLineOffsets64;
asm
  jmp pSymGetFileLineOffsets64;
end;

procedure SymGetHomeDirectory;
asm
  jmp pSymGetHomeDirectory;
end;

procedure SymGetHomeDirectoryW;
asm
  jmp pSymGetHomeDirectoryW;
end;

procedure SymGetLineFromAddr;
asm
  jmp pSymGetLineFromAddr;
end;

procedure SymGetLineFromAddr64;
asm
  jmp pSymGetLineFromAddr64;
end;

procedure SymGetLineFromAddrW64;
asm
  jmp pSymGetLineFromAddrW64;
end;

procedure SymGetLineFromName;
asm
  jmp pSymGetLineFromName;
end;

procedure SymGetLineFromName64;
asm
  jmp pSymGetLineFromName64;
end;

procedure SymGetLineFromNameW64;
asm
  jmp pSymGetLineFromNameW64;
end;

procedure SymGetLineNext;
asm
  jmp pSymGetLineNext;
end;

procedure SymGetLineNext64;
asm
  jmp pSymGetLineNext64;
end;

procedure SymGetLineNextW64;
asm
  jmp pSymGetLineNextW64;
end;

procedure SymGetLinePrev;
asm
  jmp pSymGetLinePrev;
end;

procedure SymGetLinePrev64;
asm
  jmp pSymGetLinePrev64;
end;

procedure SymGetLinePrevW64;
asm
  jmp pSymGetLinePrevW64;
end;

procedure SymGetModuleBase;
asm
  jmp pSymGetModuleBase;
end;

procedure SymGetModuleBase64;
asm
  jmp pSymGetModuleBase64;
end;

procedure SymGetModuleInfo;
asm
  jmp pSymGetModuleInfo;
end;

procedure SymGetModuleInfo64;
asm
  jmp pSymGetModuleInfo64;
end;

procedure SymGetModuleInfoW;
asm
  jmp pSymGetModuleInfoW;
end;

procedure SymGetModuleInfoW64;
asm
  jmp pSymGetModuleInfoW64;
end;

procedure SymGetOmapBlockBase;
asm
  jmp pSymGetOmapBlockBase;
end;

procedure SymGetOmaps;
asm
  jmp pSymGetOmaps;
end;

procedure SymGetOptions;
asm
  jmp pSymGetOptions;
end;

procedure SymGetScope;
asm
  jmp pSymGetScope;
end;

procedure SymGetScopeW;
asm
  jmp pSymGetScopeW;
end;

procedure SymGetSearchPath;
asm
  jmp pSymGetSearchPath;
end;

procedure SymGetSearchPathW;
asm
  jmp pSymGetSearchPathW;
end;

procedure SymGetSourceFile;
asm
  jmp pSymGetSourceFile;
end;

procedure SymGetSourceFileFromToken;
asm
  jmp pSymGetSourceFileFromToken;
end;

procedure SymGetSourceFileFromTokenW;
asm
  jmp pSymGetSourceFileFromTokenW;
end;

procedure SymGetSourceFileToken;
asm
  jmp pSymGetSourceFileToken;
end;

procedure SymGetSourceFileTokenW;
asm
  jmp pSymGetSourceFileTokenW;
end;

procedure SymGetSourceFileW;
asm
  jmp pSymGetSourceFileW;
end;

procedure SymGetSourceVarFromToken;
asm
  jmp pSymGetSourceVarFromToken;
end;

procedure SymGetSourceVarFromTokenW;
asm
  jmp pSymGetSourceVarFromTokenW;
end;

//procedure SymGetSymFromAddr;
//asm
//  jmp pSymGetSymFromAddr;
//end;

function SymGetSymFromAddr(hProcess: THandle; dwAddr: DWORD; pdwDisplacement: PDWORD; var Symbol: JclWin32.TImagehlpSymbolA): Bool; stdcall;
var
  s: ansistring;
  info: TJclLocationInfo;
begin
  Result := pSymGetSymFromAddr(hProcess, dwAddr, pdwDisplacement, Symbol);
  if not Result then
  begin
    (*
  _IMAGEHLP_SYMBOLA = packed record
    SizeOfStruct: DWORD;                   { set to sizeof(IMAGEHLP_SYMBOL) }
    Address: DWORD;                        { virtual address including dll base address }
    Size: DWORD;                           { estimated size of symbol, can be zero }
    Flags: DWORD;                          { info about the symbols, see the SYMF defines }
    MaxNameLength: DWORD;                  { maximum size of symbol name in 'Name' }
    Name: packed array[0..0] of AnsiChar;  { symbol name (null terminated string) }
  end;
    *)
//    Symbol.SizeOfStruct := SizeOf(Symbol);
//    Symbol.Address      := dwAddr;
    //Symbol.Size         :=
    //Symbol.Flags        := SYMF_OMAP_MODIFIED;
    //Symbol.MaxNameLength
    //s := 'something@nested@myclass@@';
    //s := '??0myclass@@QAE@XZ';
//    s := '?Fv_v@@YAXXZ';
//    Move(s[1], Symbol.Name[0], Length(s));
    //PAnsiChar(@Symbol.Name[0])^ := AnsiString('TEST');

    jclDebug.SetRemoteProcess(hProcess);
    if jclDebug.GetLocationInfo(Pointer(dwAddr), info) then
    begin
      Result           := True;
      Symbol.Address   := dwAddr;
      //s := 'test' + #0;
      s := info.ProcedureName + #0;
      Move(s[1], Symbol.Name[0], Length(s));  //todo: check MaxNameLen!
      //Symbol.NameLen := Length(s);
    end;
  end;
end;

procedure SymGetSymFromAddr64;
asm
  jmp pSymGetSymFromAddr64;
end;

procedure SymGetSymFromName;
asm
  jmp pSymGetSymFromName;
end;

procedure SymGetSymFromName64;
asm
  jmp pSymGetSymFromName64;
end;

procedure SymGetSymNext;
asm
  jmp pSymGetSymNext;
end;

procedure SymGetSymNext64;
asm
  jmp pSymGetSymNext64;
end;

procedure SymGetSymPrev;
asm
  jmp pSymGetSymPrev;
end;

procedure SymGetSymPrev64;
asm
  jmp pSymGetSymPrev64;
end;

procedure SymGetSymbolFile;
asm
  jmp pSymGetSymbolFile;
end;

procedure SymGetSymbolFileW;
asm
  jmp pSymGetSymbolFileW;
end;

procedure SymGetTypeFromName;
asm
  jmp pSymGetTypeFromName;
end;

procedure SymGetTypeFromNameW;
asm
  jmp pSymGetTypeFromNameW;
end;

procedure SymGetTypeInfo;
asm
  jmp pSymGetTypeInfo;
end;

procedure SymGetTypeInfoEx;
asm
  jmp pSymGetTypeInfoEx;
end;

procedure SymGetUnwindInfo;
asm
  jmp pSymGetUnwindInfo;
end;

procedure SymInitialize;
asm
  jmp pSymInitialize;
end;

procedure SymInitializeW;
asm
  jmp pSymInitializeW;
end;

procedure SymLoadModule;
asm
  jmp pSymLoadModule;
end;

procedure SymLoadModule64;
asm
  jmp pSymLoadModule64;
end;

procedure SymLoadModuleEx;
asm
  jmp pSymLoadModuleEx;
end;

procedure SymLoadModuleExW;
asm
  jmp pSymLoadModuleExW;
end;

procedure SymMatchFileName;
asm
  jmp pSymMatchFileName;
end;

procedure SymMatchFileNameW;
asm
  jmp pSymMatchFileNameW;
end;

procedure SymMatchString;
asm
  jmp pSymMatchString;
end;

procedure SymMatchStringA;
asm
  jmp pSymMatchStringA;
end;

procedure SymMatchStringW;
asm
  jmp pSymMatchStringW;
end;

procedure SymNext;
asm
  jmp pSymNext;
end;

procedure SymNextW;
asm
  jmp pSymNextW;
end;

procedure SymPrev;
asm
  jmp pSymPrev;
end;

procedure SymPrevW;
asm
  jmp pSymPrevW;
end;

procedure SymRefreshModuleList;
asm
  jmp pSymRefreshModuleList;
end;

procedure SymRegisterCallback;
asm
  jmp pSymRegisterCallback;
end;

procedure SymRegisterCallback64;
asm
  jmp pSymRegisterCallback64;
end;

procedure SymRegisterCallbackW64;
asm
  jmp pSymRegisterCallbackW64;
end;

procedure SymRegisterFunctionEntryCallback;
asm
  jmp pSymRegisterFunctionEntryCallback;
end;

procedure SymRegisterFunctionEntryCallback64;
asm
  jmp pSymRegisterFunctionEntryCallback64;
end;

procedure SymSearch;
asm
  jmp pSymSearch;
end;

procedure SymSearchW;
asm
  jmp pSymSearchW;
end;

procedure SymSetContext;
asm
  jmp pSymSetContext;
end;

procedure SymSetHomeDirectory;
asm
  jmp pSymSetHomeDirectory;
end;

procedure SymSetHomeDirectoryW;
asm
  jmp pSymSetHomeDirectoryW;
end;

procedure SymSetOptions;
asm
  jmp pSymSetOptions;
end;

procedure SymSetParentWindow;
asm
  jmp pSymSetParentWindow;
end;

procedure SymSetScopeFromAddr;
asm
  jmp pSymSetScopeFromAddr;
end;

procedure SymSetScopeFromIndex;
asm
  jmp pSymSetScopeFromIndex;
end;

procedure SymSetSearchPath;
asm
  jmp pSymSetSearchPath;
end;

procedure SymSetSearchPathW;
asm
  jmp pSymSetSearchPathW;
end;

procedure SymSrvDeltaName;
asm
  jmp pSymSrvDeltaName;
end;

procedure SymSrvDeltaNameW;
asm
  jmp pSymSrvDeltaNameW;
end;

procedure SymSrvGetFileIndexInfo;
asm
  jmp pSymSrvGetFileIndexInfo;
end;

procedure SymSrvGetFileIndexInfoW;
asm
  jmp pSymSrvGetFileIndexInfoW;
end;

procedure SymSrvGetFileIndexString;
asm
  jmp pSymSrvGetFileIndexString;
end;

procedure SymSrvGetFileIndexStringW;
asm
  jmp pSymSrvGetFileIndexStringW;
end;

procedure SymSrvGetFileIndexes;
asm
  jmp pSymSrvGetFileIndexes;
end;

procedure SymSrvGetFileIndexesW;
asm
  jmp pSymSrvGetFileIndexesW;
end;

procedure SymSrvGetSupplement;
asm
  jmp pSymSrvGetSupplement;
end;

procedure SymSrvGetSupplementW;
asm
  jmp pSymSrvGetSupplementW;
end;

procedure SymSrvIsStore;
asm
  jmp pSymSrvIsStore;
end;

procedure SymSrvIsStoreW;
asm
  jmp pSymSrvIsStoreW;
end;

procedure SymSrvStoreFile;
asm
  jmp pSymSrvStoreFile;
end;

procedure SymSrvStoreFileW;
asm
  jmp pSymSrvStoreFileW;
end;

procedure SymSrvStoreSupplement;
asm
  jmp pSymSrvStoreSupplement;
end;

procedure SymSrvStoreSupplementW;
asm
  jmp pSymSrvStoreSupplementW;
end;

procedure SymUnDName;
asm
  jmp pSymUnDName;
end;

procedure SymUnDName64;
asm
  jmp pSymUnDName64;
end;

procedure SymUnloadModule;
asm
  jmp pSymUnloadModule;
end;

procedure SymUnloadModule64;
asm
  jmp pSymUnloadModule64;
end;

procedure UnDecorateSymbolName;
asm
  jmp pUnDecorateSymbolName;
end;

procedure UnDecorateSymbolNameW;
asm
  jmp pUnDecorateSymbolNameW;
end;

procedure UnmapDebugInformation;
asm
  jmp pUnmapDebugInformation;
end;

procedure WinDbgExtensionDllInit;
asm
  jmp pWinDbgExtensionDllInit;
end;

procedure block;
asm
  jmp pblock;
end;

procedure chksym;
asm
  jmp pchksym;
end;

procedure dbghelp_;
asm
  jmp pdbghelp;
end;

procedure dh;
asm
  jmp pdh;
end;

procedure fptr;
asm
  jmp pfptr;
end;

procedure homedir;
asm
  jmp phomedir;
end;

procedure itoldyouso;
asm
  jmp pitoldyouso;
end;

procedure lmi;
asm
  jmp plmi;
end;

procedure lminfo;
asm
  jmp plminfo;
end;

procedure omap;
asm
  jmp pomap;
end;

procedure srcfiles;
asm
  jmp psrcfiles;
end;

procedure stack_force_ebp;
asm
  jmp pstack_force_ebp;
end;

procedure stackdbg;
asm
  jmp pstackdbg;
end;

procedure sym;
asm
  jmp psym;
end;

procedure symsrv;
asm
  jmp psymsrv;
end;

procedure vc7fpo;
asm
  jmp pvc7fpo;
end;

initialization
  LoadFunctions;

end.
