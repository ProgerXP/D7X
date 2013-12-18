unit ProcUtils;

interface

uses Windows, PsAPI, TlHelp32;

type
  TProcessModule = record
    Name: WideString;

    { The following fields will be 0 if GetModuleInformation has failed: }
    BaseAddress, ImageSize: DWord;
    EndAddress: DWord;    // BaseAddress + ImageSize
    EntryPoint: DWord;
  end;

  TProcessModules = array of TProcessModule;

  TProcessTokens = record
    Error: String;
    Code: DWord;
    Process, Primary: THandle;
  end;

  PImageImportDescriptor = ^TImageImportDescriptor;
  TImageImportDescriptor = packed record
    OriginalFirstThunk: DWORD;
    TimeDateStamp: DWORD;
    ForwarderChain: DWORD;
    Name: DWORD;
    FirstThunk: DWORD;
  end;

const
  THREAD_DIRECT_IMPERSONATION       = $0200;
  THREAD_GET_CONTEXT                = $0008;
  THREAD_IMPERSONATE                = $0100;
  THREAD_QUERY_INFORMATION          = $0040;
  THREAD_QUERY_LIMITED_INFORMATION  = $0800;
  THREAD_SET_CONTEXT                = $0010;
  THREAD_SET_INFORMATION            = $0020;
  THREAD_SET_LIMITED_INFORMATION    = $0400;
  THREAD_SET_THREAD_TOKEN           = $0080;
  THREAD_SUSPEND_RESUME             = $0002;
  THREAD_TERMINATE                  = $0001;

function CurrentProcessInfo: TProcessInformation;
// ProcessID = 0 returns info about current process. Result of 0 length indicates error of
// OpenProcess or EnumProcessModules.
function GetProcessModules(ProcessID: DWord = 0): TProcessModules;
// Returns True if the process has been successfully started. Returns when it's finished.
// Exe can be nil if Args contains it (regular CL format: "exe" "arg1" ...).
function RunAndWait(const Exe: WideString; const Args: WideString = '';
  ProcessFlags: DWord = CREATE_NEW_CONSOLE or NORMAL_PRIORITY_CLASS): Boolean;
function MainThreadHandleOf(ProcID, Access: DWord): DWord;
function PatchImport(ProcAddress, NewAddress: Pointer; ImageBase: DWord = 0): Boolean;

function OpenThread(dwDesiredAccess: DWord; bInheritHandle: Boolean; dwThreadId: DWord): THandle; stdcall; external kernel32;

procedure RtlCreateUserThread(ProcessHandle: THandle; SecurityDescriptor: PSecurityDescriptor;
  CreateSuspended: Bool; StackZeroBits: DWord; StackReserved: PDWord; StackCommit: PDWord;
  StartAddress: Pointer; StartParameter: Pointer; var ThreadHandle: THandle;
  ClientID: Pointer); stdcall; external 'ntdll.dll';

implementation

function CurrentProcessInfo: TProcessInformation;
begin
  Result.hProcess := GetCurrentProcess;
  Result.hThread := GetCurrentThread;
  Result.dwProcessId := GetCurrentProcessID;
  Result.dwThreadId := GetCurrentThreadID;
end;

function GetProcessModules(ProcessID: DWord = 0): TProcessModules;
var
  ProcHandle: THandle;
  ModHandles: array[0..255] of HModule;
  ArraySize: DWord;
  CurRes, I: Integer;
  ModuleName: array[0..MAX_PATH] of WideChar;
  ModuleInfo: TModuleInfo;
begin
  SetLength(Result, 0);

  if ProcessID = 0 then
    ProcessID := GetCurrentProcessId;
  ProcHandle := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, False, ProcessID);

  if (ProcHandle <> 0) and EnumProcessModules(ProcHandle, @ModHandles[0], SizeOf(ModHandles), ArraySize)
     and (ArraySize mod SizeOf(HModule) = 0) then
  begin
    CurRes := 0;

    SetLength(Result, ArraySize div SizeOf(HModule));
    ZeroMemory(@Result[0], SizeOf(Result));

    for I := 0 to ArraySize div SizeOf(HModule) - 1 do
      if GetModuleFilenameExW(ProcHandle, ModHandles[I], @ModuleName[0], SIzeOf(ModuleName)) <> 0 then
      begin
        Result[CurRes].Name := PWideChar(@ModuleName[0]);

        if GetModuleInformation(ProcHandle, ModHandles[I], @ModuleInfo, SizeOf(ModuleInfo)) then
          with Result[CurRes] do
          begin
            BaseAddress := DWord(ModuleInfo.lpBaseOfDll);
            ImageSize := ModuleInfo.SizeOfImage;
            EndAddress := BaseAddress + ImageSize;

            EntryPoint := DWord(ModuleInfo.EntryPoint);
          end;

        Inc(CurRes);
      end;

    SetLength(Result, CurRes);
  end;

  CloseHandle(ProcHandle);
end;

function RunAndWait(const Exe, Args: WideString; ProcessFlags: DWord): Boolean;
  function OrNil(const S: WideString): PWideChar;
  begin
    if S = '' then
      Result := nil
    else
      Result := PWideChar(S);
  end;

var
  StartInfo: TStartupInfo;
  ProcInfo: TProcessInformation;
  ExitCode: DWord;
begin
  ZeroMemory(@ProcInfo, SizeOf(TProcessInformation));
  StartInfo.cb := SizeOf(TStartupInfo);
  ZeroMemory(@StartInfo, SizeOf(TStartupInfo));

  Result := CreateProcessW(OrNil(Exe), PWideChar(Args), NIL, NIL,
                           False, ProcessFlags, NIL, '.', StartInfo, ProcInfo);

  if Result and (ProcInfo.hProcess <> 0) then
  begin
    Result := (GetExitCodeProcess(ProcInfo.hProcess, ExitCode) or (ExitCode = STILL_ACTIVE))
              and (WaitForSingleObject(ProcInfo.hProcess, INFINITE) = WAIT_OBJECT_0);

    CloseHandle(ProcInfo.hProcess);
    CloseHandle(ProcInfo.hThread)
  end;
end;

function MainThreadHandleOf(ProcID, Access: DWord): DWord;
var
  Snapshot: DWord;
  Entry: TThreadEntry32;
begin
  Result := 0;
  Snapshot := CreateToolhelp32Snapshot(TH32CS_SNAPTHREAD, 0);

  if Snapshot <> INVALID_HANDLE_VALUE then
  begin
    Entry.dwSize := SizeOf(Entry);

    if Thread32First(Snapshot, Entry) then
      repeat
        if Entry.th32OwnerProcessID = ProcID then
          Result := OpenThread(Access, False, Entry.th32ThreadID);
      until (Result > 0) or not Thread32Next(Snapshot, Entry);

    CloseHandle(Snapshot);
  end;
end;

function PatchImport(ProcAddress, NewAddress: Pointer; ImageBase: DWord = 0): Boolean;
var
  Old: DWord;
  PEHeader: PImageNtHeaders;
  PImport: PImageImportDescriptor;
  PRVA_Import: LPDWORD;
begin
  Result := False;

  if ImageBase = 0 then
    ImageBase := GetModuleHandle(NIL);

  PEHeader := Pointer(Int64(ImageBase) + PImageDosHeader(ImageBase)._lfanew);
  PImport := Pointer(PEHeader.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_IMPORT].VirtualAddress + ImageBase);

  while PImport.Name <> 0 do
  begin
    PRVA_Import := LPDWORD(pImport.FirstThunk + ImageBase);

    while PRVA_Import^ <> 0 do
    begin
      if (PPointer(PRVA_Import)^ = ProcAddress) and
         VirtualProtect(PPointer(PRVA_Import), 4, PAGE_READWRITE, Old) then
      begin
        Result := True;

        PPointer(PRVA_Import)^ := NewAddress;
        VirtualProtect(PPointer(PRVA_Import), 4, Old, Old);
      end;

      Inc(PRVA_Import);
    end;

    Inc(PImport);
  end;
end;

end.