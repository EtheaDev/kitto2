{-------------------------------------------------------------------------------
   Copyright 2012 Ethea S.r.l.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
-------------------------------------------------------------------------------}

///	<summary>
///	  This unit gathers system-related routines and classes.<br />Services
///	  provided by this unit fall in these categories:<br />- File services.
///	  <br />- Process management services.<br />- Windows Registry access.
///	  <br />- Environment and localization.<br />- Security and network
///	  services.
///	</summary>
unit EF.SysUtils;

{$I EF.Defines.inc}

interface

uses
  Windows, Classes, RTLConsts, SysUtils, Registry, Types;

///	<summary>
///	  Tries to open the file AFileName for writing, and returns False in case
///	  of errors. Use it to know whether a file can be deleted/overwritten or
///	  not.
///	</summary>
function IsFileInUse(const AFileName: string): Boolean;

///	<summary>
///	  Returns the number of files in the given folder. APatch should be an
///	  absolute or relative path name, with or without trailing path delimiter.
///	</summary>
function GetFileCount(const APath: string): Integer;

///	<summary>
///	  Returns the size of the named file without opening the file. If the file
///	  doesn't exist, returns -1.
///	</summary>
///	<remarks>
///	  Currently this function doesn't support files bigger than 2 GBs.
///	</remarks>
function GetFileSize(const AFileName: string): Longint;

///	<summary>
///	  Returns AFileName's OS timestamp.
///	</summary>
function GetFileDateTime(const AFileName: string): TDateTime;

///	<summary>
///	  Removes a trailing path delimiter from APath, if present. It's the
///	  opposite of SysUtils.IncludeTrailingPathDelimiter.
///	</summary>
function RemoveTrailingPathDelimiter(const APath: string): string;

///	<summary>
///	  Extract the file format from AFileName. The file format is the file
///	  extension without the leading '.'.
///	</summary>
function ExtractFileFormat(const AFileName: string): string;

///	<summary>
///	  Tells whether APath is an absolute (True) or relative (False) path.
///	</summary>
function IsAbsolutePath(const APath: string): Boolean;

///	<summary>
///	  Appends to AFileNames the names of all files of the given format found in
///	  ARootPath. By default includes also the files found in ARootPath's
///	  subfolders.
///	</summary>
///	<remarks>
///	  This function is a wrapper around TEFFileLister. If you need more
///	  flexibility then use TEFFileLister directly, or write a different wrapper.
///	</remarks>
procedure FindAllFiles(const AFileFormat, ARootPath: string; const AFileNames: TStrings;
  const AIncludeSubFolders: Boolean = True; const AFullPaths: Boolean = True); overload;

///	<summary>
///	  Appends to AFileNames the names of all files of one of the given formats
///	  found in ARootPath. By default includes also the files found in
///	  ARootPath's subfolders.
///	</summary>
///	<remarks>
///	  This function is a wrapper around TEFFileLister. If you need more
///	  flexibility then use TEFFileLister directly, or write a different wrapper.
///	</remarks>
procedure FindAllFiles(const AFileFormats: array of string; const ARootPath: string;
  const AFileNames: TStrings; const AIncludeSubFolders: Boolean = True;
  const AFullPaths: Boolean = True); overload;

type
  TDirectoryProc = TProc<string>;

///	<summary>Calls the specified procedure once for each directory directly
///	underneath the specified root path.</summary>
///	<remarks>This function is a wrapper around TEFFileLister. If you need more
///	flexibility then use TEFFileLister directly, or write a different
///	wrapper.</remarks>
procedure EnumDirectories(const ARootPath: string; const AProc: TDirectoryProc);

///	<summary>Returns True if the specified directory exists and is empty, False
///	otherwise.</summary>
function IsDirectoryEmpty(const APath: string): Boolean;

///	<summary>
///	  Executes an application synchronously (AWait = True) or Asynchronously
///	  (AWait = False). If AWait is True, the function waits that the launched
///	  process finishes, and returns the process' exit code (or -1 in case of
///	  errors). If AWait is False, the function returns 0 if the call succeeds
///	  or -1 in case of errors.
///	</summary>
function ExecuteApplication(const AFileName: string; const AWait: Boolean = False): Integer; overload;

///	<summary>
///	  Executes an application synchronously and returns the application's exit
///	  code (or -1 in case of errors) and the console output. Use this function
///	  to execute console applications and command files. The application is run
///	  in hidden state, so that no console window is displayed.
///	</summary>
function ExecuteApplication(const AFileName: string; const AOutput: TStrings): Integer; overload;

///	<summary>
///	  Executes an application synchronously (AWait = True) or Asynchronously
///	  (AWait = False), with a specified working directory.
///	</summary>
function ExecuteApplication(const AFileName, AWorkingDirectory: string; const AWait: Boolean = False): Integer; overload;

///	<summary>
///	  Executes an application asynchronously, but the process handle is not
///	  closed. Instead, it is retained and returned so that, for example,
///	  TerminateApplication can be called later. The caller is responsible for
///	  closing the process handle.
///	</summary>
function ExecuteApplication(const AFileName: string; out AProcessHandle: Cardinal): Integer; overload;

///	<summary>
///	  Terminates the application and returns True. Returns False in case of an
///	  error.
///	</summary>
function TerminateApplication(const AProcessHandle: Cardinal): Boolean;

///	<summary>
///	  Wraps SetProcessWorkingSetSize. See MSDN to know more about it. Call this
///	  function periodically, or once after application startup, to unmap all
///	  unused pages from the process' virtual memory space. This is the same
///	  thing that happens on NT-based systems when the main window of an
///	  application is minimized, and it is done to save memory.
///	</summary>
procedure TrimProcessWorkingSet;

///	<summary>
///	  Tries to return as much memory as possible to the OS, if the current
///	  process' working set is greater than the specified number of bytes. This
///	  reduces memory fragmentation in long running processes, such as services.
///	  This function is inexpensive and it can be called very often. The cost of
///	  shrinking the memory set will only occurr when it is greater than the
///	  specified amount. Don't specify irrealistically lower amounts. Example:
///	  EmptyProcessWorkingSet(200 * 1024 *1024);
///	</summary>
procedure ShrinkProcessWorkingSet(const AMaxMemory: Cardinal);

///	<summary>
///	  Deletes a Registry key and all its subkeys. Returns False in case of
///	  errors. Returns True anyway if the key doesn't exist.
///	</summary>
function RegDeleteKeyAndSubkeys(const AParentKey: TRegistry; const AKeyToDelete: string): Boolean;

///	<summary>
///	  Returns the network name of the machine.
///	</summary>
function GetMachineName: string;

///	<summary>
///	  Returns the name of the currently logged on user.
///	</summary>
function GetUserName: string;

///	<summary>
///	  Expands all environment variables found in AString and returns the
///	  resulting string. It is a wrapper around the ExpandEnvironmentStrings API
///	  function. Environment variable names should be enclosed between %
///	  characters and are case-insensitive.
///	</summary>
function ExpandEnvironmentVariables(const AString: string): string;

///	<summary>
///	  Wraps GetWindowsDirectory. Raises exceptions in case of buffer overflows.
///	</summary>
function SafeGetWindowsDirectory: string;

///	<summary>
///	  Wraps GetSystemDirectory. Raises exceptions in case of buffer overflows.
///	</summary>
function SafeGetSystemDirectory: string;

///	<summary>
///	  Return the current user's temporary directory, including the trailing
///	  path delimiter.
///	</summary>
function GetTempDirectory: string;

///	<summary>
///	  Returns the randomly generated name of a non-existing file in the
///	  system's temporary directory. You can pass a custom extension, otherwise
///	  the file name will have .tmp extension.
///	</summary>
function GetTempFileName(const AFileExtension: string = '.tmp'): string;

///	<summary>
///	  Scans the command line and returns the value of the parameter that
///	  follows the one with the specificed name (with leading - or /). If the
///	  parameter is not found, ADefaultValue is returned.
///	</summary>
function GetCmdLineParamValue(const AParamName: string; const ADefaultValue: string = ''): string;

///	<summary>Returns the full path of the Program Files(x86) directory, or the
///	Program Files directory if the other does not exist.</summary>
function GetProgramFilesx86Directory: string;

///	<summary>Returns the full path of the Program Files directory.</summary>
function GetProgramFilesDirectory: string;

///	<summary>
///	  Returns a random file name which doesn't exists in APath and has
///	  extension AExtension. Keeps generating random names until it finds a free
///	  name.
///	</summary>
function GetUniqueFileName(const APath, AExtension: string): string;

///	<summary>
///	  Returns a new TFormatSettings records in a manner that unifies syntax for
///	  different Delphi versions. Use it instead of GetLocaleFormatSettings or
///	  TFormatSettings.Create.
///	</summary>
function GetFormatSettings: TFormatSettings;

type
  TEFFileErrorAction = (eaRetry, eaSkip, eaAbort, eaFail);

  TEFBeforeProcessFileProc = reference to procedure (const ASourceFileName: string;
    var ADestinationFileName: string; var AAllow: Boolean);

  TEFProcessFileProc = reference to procedure (const ASourceFileName,
    ADestinationFileName: string);

  TEFProcessFileErrorProc = reference to procedure (const ASourceFileName,
    ADestinationFileName: string; const AError: Exception;
    var AAction: TEFFileErrorAction);

  ///	<summary>
  ///	  Abstract base class for classes that do things to a file or a set of
  ///	  files contained in a folder or in two parallel folder hierarchies.
  ///	</summary>
  TEFFileProcessor = class
  private
    FRecurseSubdirs: Boolean;
    FSourcePath: string;
    FDestinationPath: string;
    FOnProcessFileError: TEFProcessFileErrorProc;
    FFileMask: string;
    FRetryCount: Integer;
    FRetryDelay: Integer;
    FAfterProcessFile: TEFProcessFileProc;
    FBeforeProcessFile: TEFBeforeProcessFileProc;
    FDefaultErrorAction: TEFFileErrorAction;
    FExceptions: TStrings;
    procedure DoProcess(const ASourcePath, ADestinationPath: string);
    procedure SetExceptions(const AValue: TStrings);
  public
    constructor Create; virtual;
    destructor Destroy; override;
  protected
    ///	<summary>
    ///	  Fires OnProcessFileError.
    ///	</summary>
    function DoProcessFileError(const E: Exception; const ASourceFileName,
      ADestinationFileName: string): TEFFileErrorAction;

    ///	<summary>
    ///	  Starts processing. Might be renamed (wrapped) in descendant.
    ///	</summary>
    procedure Process;

    ///	<summary>
    ///	  Process a single file. Ignores SourcePath and DestinationPath and
    ///	  invokes the operation on the specified file(s).
    ///	</summary>
    procedure ProcessFile(const ASourceFileName, ADestinationFileName: string);

    ///	<summary>
    ///	  Implements the file process operation. Should be overridden in
    ///	  descendants.
    ///	</summary>
    procedure DoProcessFile(const ASourceFileName, ADestinationFileName: string); virtual; abstract;

    ///	<summary>
    ///	  Called at the beginning of Process. This method should raise
    ///	  exceptions if needed parameters are not specified or not valid.
    ///	</summary>
    procedure CheckPreconditions; virtual;

    ///	<summary>
    ///	  Called after processing each folder. ADirectioryName does NOT contain
    ///	  any trailing PathDelim.
    ///	</summary>
    procedure AfterProcessDirectory(const ADirectoryName: string); virtual;
  public
  const
    DEFAULT_RETRY_COUNT = 5;
    DEFAULT_RETRY_DELAY = 2000;
    DEFAULT_RECURSE_SUBDIRS = True;
    DEFAULT_FILE_MASK = '*.*';
    DEFAULT_DEFAULT_ERROR_ACTION = eaFail;

    ///	<summary>
    ///	  Locates the files to process. Operations that work on two folders
    ///	  (such as the copy operation) use both SourcePath and DestinationPath,
    ///	  while operations that work on one folder only (such as the delete
    ///	  operation) only need and use SourcePath.
    ///	</summary>
    property SourcePath: string read FSourcePath write FSourcePath;

    ///	<summary>
    ///	  Locates the files to process. Operations that work on two folders
    ///	  (such as the copy operation) use both SourcePath and DestinationPath,
    ///	  while operations that work on one folder only (such as the delete
    ///	  operation) only need and use SourcePath.
    ///	</summary>
    property DestinationPath: string read FDestinationPath write FDestinationPath;

    ///	<summary>
    ///	  When True, causes the object to process ASourcePath's subfolders
    ///	  recursively.
    ///	</summary>
    ///	<remarks>
    ///	  Setting this property to True is incompatible with any value for
    ///	  FileMask except '*.*'.
    ///	</remarks>
    property RecurseSubdirs: Boolean read FRecurseSubdirs write FRecurseSubdirs
      default DEFAULT_RECURSE_SUBDIRS;

    ///	<summary>
    ///	  Set this property to filter certain file types only. Supports the
    ///	  wildcards * and ?.
    ///	</summary>
    property FileMask: string read FFileMask write FFileMask; // default DEFAULT_FILE_MASK

    ///	<summary>
    ///	  This event is fired in case of errors, after whatever number of
    ///	  retries specified in RetryCount. The handler receives a reference
    ///	  parameter called Action which may be set to one of the following
    ///	  values:<br />eaRetry: Immediately retry the operation on the current
    ///	  file;<br />eaSkip: Skip the current file and process the next file;
    ///	  <br />eaAbort: Raise a silent exception (EAbort);<br />eaFail:
    ///	  Re-raise the original error.
    ///	</summary>
    property OnProcessFileError: TEFProcessFileErrorProc read FOnProcessFileError
      write FOnProcessFileError;

    ///	<summary>
    ///	  Default action in case of errors. When OnProcessFileError is handled,
    ///	  this is the value passed to the handler, otherwise it's the value
    ///	  used to decide what to do.
    ///	</summary>
    property DefaultErrorAction: TEFFileErrorAction read FDefaultErrorAction
      write FDefaultErrorAction default DEFAULT_DEFAULT_ERROR_ACTION;

    ///	<summary>
    ///	  Fired before processing each file. The handler receives the file
    ///	  name(s) in input.
    ///	</summary>
    property BeforeProcessFile: TEFBeforeProcessFileProc read FBeforeProcessFile
      write FBeforeProcessFile;

    ///	<summary>
    ///	  Fired after processing each file. The handler receives the file
    ///	  name(s) in input.
    ///	</summary>
    property AfterProcessFile: TEFProcessFileProc read FAfterProcessFile
      write FAfterProcessFile;

    ///	<summary>
    ///	  Indicates how many times a failed operation should be retried.
    ///	</summary>
    property RetryCount: Integer read FRetryCount write FRetryCount
      default DEFAULT_RETRY_COUNT;

    ///	<summary>
    ///	  Indicates how many milliseconds to wait between a try and the next
    ///	  retry in case of errors.
    ///	</summary>
    property RetryDelay: Integer read FRetryDelay write FRetryDelay
      default DEFAULT_RETRY_DELAY;

    ///	<summary>
    ///	  A list of exclusion patterns. Any file whose name matches one of the
    ///	  exceptions is not processed.
    ///	</summary>
    property Exceptions: TStrings read FExceptions write SetExceptions;
  end;

  ///	<summary>
  ///	  Abstract base class for classes that do things to a file or a set of
  ///	  files contained in a single folder.
  ///	</summary>
  TEFSourcePathOnlyFileProcessor = class(TEFFileProcessor)
  end;

  TEFDeleteDirectoryNotifyEvent = procedure (Sender: TObject; const DirectoryName: string) of object;

  ///	<summary>
  ///	  Deletes a file or a group of files contained in a folder.
  ///	</summary>
  TEFFileDeleter = class(TEFSourcePathOnlyFileProcessor)
  private
    FDeleteEmptyRootFolder: Boolean;
    FDeleteEmptySubfolders: Boolean;
    FAfterDeleteDirectory: TEFDeleteDirectoryNotifyEvent;
    FBeforeDeleteDirectory: TEFDeleteDirectoryNotifyEvent;
  protected
    ///	<summary>
    ///	  Deletes the specified folder by calling DoDeleteDirectory; handles
    ///	  errors, retries, retry delays.
    ///	</summary>
    procedure DeleteDirectory(const ADirectoryName: string);

    ///	<summary>
    ///	  Tries to delete the specified folder; raises exceptions if it can't.
    ///	</summary>
    procedure DoDeleteDirectory(const ADirectoryName: string);

    ///	<summary>
    ///	  Implements the deletion of ASourceFileName.
    ///	</summary>
    procedure DoProcessFile(const ASourceFileName, ADestinationFileName: string); override;

    ///	<summary>
    ///	  Ensures any empty directories are deleted after deleting the files
    ///	  within, if required.
    ///	</summary>
    procedure AfterProcessDirectory(const ADirectoryName: string); override;
  public
  const
    DEFAULT_DELETE_EMPTY_ROOT_FOLDER = False;
    DEFAULT_DELETE_EMPTY_SUBFOLDERS = True;

    ///	<summary>
    ///	  Fired before deleting a folder.
    ///	</summary>
    property BeforeDeleteDirectory: TEFDeleteDirectoryNotifyEvent read FBeforeDeleteDirectory
      write FBeforeDeleteDirectory;

    ///	<summary>
    ///	  Fired after a folder has been deleted.
    ///	</summary>
    property AfterDeleteDirectory: TEFDeleteDirectoryNotifyEvent read FAfterDeleteDirectory
      write FAfterDeleteDirectory;

    ///	<summary>
    ///	  Set this property to True to cause the folder indicated by SourcePath
    ///	  to be deleted, but only if it remains empty after deleting all
    ///	  requested files and folders.
    ///	</summary>
    property DeleteEmptyRootFolder: Boolean read FDeleteEmptyRootFolder write FDeleteEmptyRootFolder
      default DEFAULT_DELETE_EMPTY_ROOT_FOLDER;

    ///	<summary>
    ///	  Set this property to True to always delete a folder once all files in
    ///	  it have been deleted. Warning: if you set a FileMask that doesn't
    ///	  cause the deletion of all files, and set this property to True,
    ///	  you'll cause a potentially endless loop of OnProcessFileError events,
    ///	  since you can only delete a folder if it's empty.
    ///	</summary>
    property DeleteEmptySubfolders: Boolean read FDeleteEmptySubfolders
      write FDeleteEmptySubfolders default DEFAULT_DELETE_EMPTY_SUBFOLDERS;

    ///	<summary>
    ///	  Deletes all files that match the settings.
    ///	</summary>
    procedure DeleteFiles;

    ///	<summary>
    ///	  Deletes a single file.
    ///	</summary>
    procedure DeleteFile(const AFileName: string);
  end;

  ///	<summary>
  ///	  Makes sure that a file (or a group of files) is writable and is not in
  ///	  use. That is, it can be overwritten.
  ///	</summary>
  TEFFileChecker = class(TEFSourcePathOnlyFileProcessor)
  protected
    ///	<summary>
    ///	  Implements the overwrite check for ASourceFileName.
    ///	</summary>
    procedure DoProcessFile(const ASourceFileName, ADestinationFileName: string); override;
  public
    ///	<summary>
    ///	  Checks all files that match the settings.
    ///	</summary>
    procedure CheckFiles;

    ///	<summary>
    ///	  Checks a single file.
    ///	</summary>
    procedure CheckFile(const AFileName: string);
  end;

  ///	<summary>
  ///	  Writes some text content to a file. Works one file at a time. An
  ///	  existing file is deleted and rewritten.
  ///	</summary>
  TEFFileWriter = class(TEFSourcePathOnlyFileProcessor)
  private
    FFileContent: string;
  protected

    ///	<summary>
    ///	  Implements content writing on ASourceFileName.
    ///	</summary>
    procedure DoProcessFile(const ASourceFileName, ADestinationFileName: string); override;
  public
    ///	<summary>
    ///	  Writes AContent to the specified file. If the file already exists it
    ///	  is deleted before writing.
    ///	</summary>
    procedure WriteFile(const AFileName, AFileContent: string);
  end;

  ///	<summary>
  ///	  Lists all files of a given format (or array of formats) in a folder or
  ///	  folder hierarchy.
  ///	</summary>
  TEFFileLister = class(TEFSourcePathOnlyFileProcessor)
  private
    FFileFormats: TStringDynArray;
    FFileNameList: TStrings;
    FFullPaths: Boolean;
  protected

    ///	<summary>
    ///	  Adds ASourceFileName to the internal list, if allowed.
    ///	</summary>
    procedure DoProcessFile(const ASourceFileName, ADestinationFileName: string); override;
  public
    ///	<summary>
    ///	  Adds the names of all files of the given formats to AFileNameList. If
    ///	  AFullPaths is True (or RecurseSubDirs is True) then the added names
    ///	  are full path names.
    ///	</summary>
    ///	<param name="AFileNameList">
    ///	  List that will hold the file names.
    ///	</param>
    ///	<param name="AFileFormats">
    ///	  An array of file formats, which are file extensions without the dot.
    ///	  The special file format '*', meaning all file formats, is supported.
    ///	</param>
    ///	<param name="AFullPaths">
    ///	  Pass True to get the full path names, False for just the file names.
    ///	</param>
    ///	<returns>
    ///	  Returns the number of items added.
    ///	</returns>
    function ListFiles(const AFileNameList: TStrings;
      const AFileFormats: TStringDynArray;
      const AFullPaths: Boolean = True): Integer;
  end;

  ///	<summary>
  ///	  Abstract base class for classes that do things to a file or a set of
  ///	  files contained in two parallel folder hierarchies.
  ///	</summary>
  TEFSourceDestPathFileProcessor = class(TEFFileProcessor)
  protected

    ///	<summary>
    ///	  Checks that the destination file name is specified as well.
    ///	</summary>
    procedure CheckPreconditions; override;
  end;

  ///	<summary>
  ///	  Copies a file or a group of files and folders.
  ///	</summary>
  TEFFileCopier = class(TEFSourceDestPathFileProcessor)
  protected

    ///	<summary>
    ///	  Implements the file copy operation.
    ///	</summary>
    procedure DoProcessFile(const ASourceFileName, ADestinationFileName: string); override;
  public

    ///	<summary>
    ///	  Copies all files that match the settings.
    ///	</summary>
    procedure CopyFiles;

    ///	<summary>
    ///	  Copies a single file.
    ///	</summary>
    procedure CopyFile(const ASourceFileName, ADestinationFileName: string);
  end;

///	<summary>
///	  Deletes the specified file. It is a simple wrapper around TEFFileDeleter.
///	  If you need more flexibility then use TEFFileDeleter directly or write a
///	  different wrapper.
///	</summary>
procedure DeleteFile(const AFileName: string);

///	<summary>
///	  Deletes all files and folders in APath (but not APath itself). It is a
///	  simple wrapper around TEFFileDeleter. If you need more flexibility then
///	  use TEFFileDeleter directly or write a different wrapper.
///	</summary>
procedure DeleteAllFiles(const APath: string);

///	<summary>
///	  Deletes APath with all files and folders therein. It is a
///	  simple wrapper around TEFFileDeleter. If you need more flexibility then
///	  use TEFFileDeleter directly or write a different wrapper.
///	</summary>
procedure DeleteTree(const APath: string);

///	<summary>
///	  Copies the specified file. It is a simple wrapper around TFileCopier. If
///	  you need more flexibility then use TEFFileCopier directly or write a
///	  different wrapper.
///	</summary>
procedure CopyFile(const ASourceFileName, ADestinationFileName: string);

///	<summary>
///	  Copies all files and folders from ASourcePath to ADestinationPath. It is
///	  a simple wrapper around TFileCopier. If you need more flexibility then
///	  use TFileCopier directly or write a different wrapper.
///	</summary>
procedure CopyAllFilesAndFolders(const ASourcePath, ADestinationPath: string;
  const ABeforeEachFile: TEFBeforeProcessFileProc = nil;
  const AAfterEachFile: TEFProcessFileProc = nil);

///	<summary>
///	  Copies all files and folders from ASourcePath to ADestinationPath, except
///	  those matching the patterns specified in AExceptions. It is a simple
///	  wrapper around TEFFileCopier. If you need more flexibility then use
///	  TFileCopier directly or write a different wrapper.
///	</summary>
procedure CopyAllFilesAndFoldersExcept(
  const ASourcePath, ADestinationPath: string;
  const AExceptions: array of string;
  const ABeforeEachFile: TEFBeforeProcessFileProc = nil;
  const AAfterEachFile: TEFProcessFileProc = nil);

///	<summary>
///	  Checks that the specified file can be written to, otherwise raises an
///	  exception. It is a simple wrapper around TEFFileChecker. If you need more
///	  flexibility then use TEFFileChecker directly or write a different wrapper.
///	</summary>
procedure CheckFileInUse(const AFileName: string);

///	<summary>
///	  Returns True if two strings refer to the same directory. Both must exist
///	  for this function to work.
///	</summary>
function SameDirectory(const ADirectory1, ADirectory2: string): Boolean;

///	<summary>
///	  Returns the number of bytes in the current process' working set.
///	</summary>
function GetCurrentProcessMemory: Cardinal;


///	<summary>Tries to determine the type of the data stream by interpreting the
///	first few bytes of the specified binary content. Currently supports several
///	graphic image types and the pdf type.</summary>
///	<param name="ABytes">Data stream. No need to pass all data; the first 10
///	bytes are enough. If not enough data is passed, the default type is
///	returned.</param>
///	<param name="ADefault">Default value that will be returned if the data
///	stream is not recognized.</param>
///	<returns>A lowercase string (such as 'gif' or 'pdf') suitable as a file
///	extension (without the dot).</returns>
function GetDataType(const ABytes: TBytes; const ADefault: string): string;

implementation

uses
  Math, SysConst, StrUtils, PsAPI,
  EF.Localization, EF.StrUtils;

function IsFileInUse(const AFileName: string): Boolean;
begin
  try
    TFileStream.Create(AFileName, fmOpenWrite or fmShareExclusive).Free;
    Result := False;
  except
    on E: EFOpenError do
      Result := True;
  end;
end;

{
  Executes an application.
  AFileName is the executable name, optionally complete with full path.
  AVisibility is one of the SW_* constants defined in the unit Windows.
  If AWait is True, the function waits that the launched process finishes,
  and returns the process' exit code (or -1 in case of errors).
  If AWait is False, the function returns 0 if the call succeeds or -1 in case
  of errors.
  If ARetainProcessHandle is True, AProcessHandle contains the launched process'
  handle. CAUTION: in this case, the caller is responsible for freeing the
  handle (by passing it to the CloseHandle() Win32 API function) when it is no
  longer required.
  The application is started from the current folder, unless a different one is
  specified in ACurrentDirectory.
  If you pass a string list in AOutput, and True in AWait, then the
  application's console output is redirected to a temporary file which is
  later loaded into AOutput and deleted. Use this mode for console applications
  and batch files only.
}
function InternalExecuteApplication(const AFileName: string;
  const AVisibility: Integer; const AWait: Boolean;
  const ARetainProcessHandle: Boolean; out AProcessHandle: Cardinal;
  const ACurrentDirectory: string = '';
  const AOutput: TStrings = nil): Integer;
var
  LApplicationName: array[0..511] of char;
  LCurrentDirectory: array[0..511] of char;
  LPCurrentDirectory: PChar;
  LStartupInfo: TStartupInfo;
  LProcessInfo: TProcessInformation;
  LCreateProcessReturnValue: Boolean;
  LUnsignedResult: Cardinal;
  LOutputTempFileName: string;
  LOutputTempFileHandle: THandle;
  LOutputTempFileSecurityAttributes: TSecurityAttributes;
  LUseOutputTempFile: Boolean;
begin
  Result := 0;
  LOutputTempFileName := '';
  LUseOutputTempFile := Assigned(AOutput) and AWait;
  LOutputTempFileHandle := 0;
  try
    AProcessHandle := 0;
    StrPCopy(LApplicationName, AFileName);
    if ACurrentDirectory <> '' then
    begin
      StrPCopy(LCurrentDirectory, ACurrentDirectory);
      LPCurrentDirectory := LCurrentDirectory;
    end
    else
      LPCurrentDirectory := nil;

    if LUseOutputTempFile then
    begin
      FillChar(LOutputTempFileSecurityAttributes, SizeOf(LOutputTempFileSecurityAttributes), #0);
      LOutputTempFileSecurityAttributes.nLength := SizeOf(LOutputTempFileSecurityAttributes);
      LOutputTempFileSecurityAttributes.lpSecurityDescriptor := nil;
      LOutputTempFileSecurityAttributes.bInheritHandle := True;

      LOutputTempFileName := GetTempFileName;
      LOutputTempFileHandle := CreateFile(PChar(LOutputTempFileName),
        GENERIC_READ or GENERIC_WRITE, FILE_SHARE_READ or FILE_SHARE_WRITE,
        @LOutputTempFileSecurityAttributes, CREATE_ALWAYS,
        FILE_ATTRIBUTE_TEMPORARY or FILE_FLAG_WRITE_THROUGH, 0);
      if LOutputTempFileHandle = INVALID_HANDLE_VALUE then
        raise Exception.CreateFmt('Couldn''t write temporary file "%s".', [LOutputTempFileHandle]);
    end;

    FillChar(LStartupInfo, SizeOf(LStartupInfo), #0);
    LStartupInfo.cb := SizeOf(LStartupInfo);
    LStartupInfo.dwFlags := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
    LStartupInfo.wShowWindow := AVisibility;
    if LUseOutputTempFile then
    begin
      LStartupInfo.hStdInput := GetStdHandle(STD_INPUT_HANDLE);
      LStartupInfo.hStdError := LOutputTempFileHandle;
      LStartupInfo.hStdOutput := LOutputTempFileHandle;
    end;
    LCreateProcessReturnValue := CreateProcess(
      nil, LApplicationName, nil, nil, LUseOutputTempFile,
      CREATE_NEW_CONSOLE or NORMAL_PRIORITY_CLASS,
      nil, LPCurrentDirectory, LStartupInfo, LProcessInfo);
    if LCreateProcessReturnValue then
    begin
      CloseHandle(LProcessInfo.hThread);
      if AWait and not ARetainProcessHandle then
      begin
        WaitForSingleObject(LProcessInfo.hProcess, INFINITE);
        GetExitCodeProcess(LProcessInfo.hProcess, LUnsignedResult);
        Result := LUnsignedResult;
      end
      else
        Result := 0;
      if ARetainProcessHandle then
        AProcessHandle := LProcessInfo.hProcess
      else
        CloseHandle(LProcessInfo.hProcess);
    end
    else
      Result := -1;

    if LUseOutputTempFile then
    begin
      CloseHandle(LOutputTempFileHandle);
      if FileExists(LOutputTempFileName) then
        AOutput.LoadFromFile(LOutputTempFileName);
    end;
    
  finally
    if (LOutputTempFileName <> '') and FileExists(LOutputTempFileName) then
      DeleteFile(LOutputTempFileName);
  end;
end;

function ExecuteApplication(const AFileName: string; const AWait: Boolean = False): Integer;
var
  LDummy: Cardinal;
begin
  Result := InternalExecuteApplication(AFileName, SW_NORMAL, AWait, False, LDummy);
end;

function ExecuteApplication(const AFileName: string; const AOutput: TStrings): Integer;
var
  LDummy: Cardinal;
begin
  Result := InternalExecuteApplication(AFileName, SW_HIDE, True, False, LDummy, '', AOutput);
end;

function ExecuteApplication(const AFileName, AWorkingDirectory: string;
  const AWait: Boolean = False): Integer; overload;
var
  LDummy: Cardinal;
begin
  Result := InternalExecuteApplication(AFileName, SW_NORMAL, AWait, False, LDummy, AWorkingDirectory);
end;

function ExecuteApplication(const AFileName: string; out AProcessHandle: Cardinal): Integer;
begin
  Result := InternalExecuteApplication(AFileName, SW_NORMAL, False, True, AProcessHandle);
end;

function TerminateApplication(const AProcessHandle: Cardinal): Boolean;
begin
  Result := TerminateProcess(AProcessHandle, 0);
end;

function RegDeleteKeyAndSubkeys(const AParentKey: TRegistry;
  const AKeyToDelete: string): Boolean;
var
  LSubKeys: TStrings;
  LSubKeyIndex: Integer;
begin
  LSubKeys := TStringList.Create;
  try
    Result := False;
    with AParentKey do
    begin
      if KeyExists(AKeyToDelete) then
      begin
        // Try to eliminate the key. Under Win9x this should eliminate
        // all the subkeys as well.
        if not DeleteKey(AKeyToDelete) then
        begin
          // If the previous call failed, then we're not on Win9x so we call
          // the function recursively for all the subkeys.
          OpenKey(AKeyToDelete, False);
          LSubKeys.Clear;
          GetKeyNames(LSubKeys);
          for LSubKeyIndex := 0 to LSubKeys.Count - 1 do
            Result := RegDeleteKeyAndSubkeys(AParentKey, LSubKeys[LSubKeyIndex]);
        end;
      end
      else
        Result := True;
    end;
  finally
    LSubKeys.Free;
  end;
end;

function GetFileCount(const APath: string): Integer;
var
  LSearchRec: TSearchRec;
begin
  Result := 0;
  if FindFirst(IncludeTrailingPathDelimiter(APath) + '*.*', 0, LSearchRec) = 0 then
  begin
    try
      Inc(Result);
      while FindNext(LSearchRec) = 0 do
        Inc(Result);
    finally
      FindClose(LSearchRec);
    end;
  end;
end;

function GetFileSize(const AFileName: string): Longint;
var
  LSearchRec: TSearchRec;
begin
  if FindFirst(ExpandFileName(AFileName), faAnyFile, LSearchRec) = 0 then
    Result := LSearchRec.Size
  else
    Result := -1;
end;

function GetFileDateTime(const AFileName: string): TDateTime;
begin
  {$IF CompilerVersion < 18.0}
  Result := FileDateToDateTime(FileAge(AFileName));
  {$ELSE}
  if not FileAge(AFileName, Result) then
    raise Exception.CreateFmt(('Couldn''t determine the age of file "%s".'), [AFileName])
  {$IFEND}
end;

procedure TrimProcessWorkingSet;
var
  LMainHandle: THandle;
begin
  LMainHandle := OpenProcess(PROCESS_ALL_ACCESS, False, GetCurrentProcessID);
  SetProcessWorkingSetSize(LMainHandle, $FFFFFFFF, $FFFFFFFF);
  CloseHandle(LMainHandle);
end;

function GetMachineName: string;
var
  LBuffer: array[0..255] of Char;
  LSize: LongWord;
begin
  LSize := SizeOf(LBuffer);
  if GetComputerName(LBuffer, LSize) then
    Result := LBuffer
  else
    Result := '';
end;

function GetUserName: string;
var
  LBuffer: array[0..255] of Char;
  LSize: LongWord;
begin
  LSize := SizeOf(LBuffer);
  if Windows.GetUserName(LBuffer, LSize) then
    Result := LBuffer
  else
    Result := '';
end;

function ExpandEnvironmentVariables(const AString: string): string;
var
  LPos1: Integer;
  LPos2: Integer;
  LVariableValue: string;
begin
  Result := AString;
  LPos1 := Pos('%', AString);
  if LPos1 > 0 then
  begin
    LPos2 := PosEx('%', AString, LPos1 + 1);
    if LPos2 > 0 then
    begin
      LVariableValue := GetEnvironmentVariable(Copy(Result, LPos1 + 1, LPos2 - LPos1 - 1));
      if LVariableValue <> '' then
        Result := Copy(Result, 1, LPos1 - 1) + LVariableValue
          + ExpandEnvironmentVariables(Copy(Result, LPos2 + 1, MaxInt))
      else
        Result := Copy(Result, 1, LPos2)
          + ExpandEnvironmentVariables(Copy(Result, LPos2 + 1, MaxInt));
    end;
  end;
end;

function SafeGetWindowsDirectory: string;
var
  LBuffer: array[0..MAX_PATH] of Char;
  LCharCount: Longword;
begin
  FillChar(LBuffer, SizeOf(LBuffer), #0);
  LCharCount := GetWindowsDirectory(LBuffer, SizeOf(LBuffer));
  if LCharCount > SizeOf(LBuffer) then
    raise Exception.Create(_('SafeGetWindowsDirectory: buffer too small.'));
  Result := LBuffer;
end;

function SafeGetSystemDirectory: string;
var
  LBuffer: array[0..MAX_PATH] of Char;
  LCharCount: Longword;
begin
  FillChar(LBuffer, SizeOf(LBuffer), #0);
  LCharCount := GetSystemDirectory(LBuffer, SizeOf(LBuffer));
  if LCharCount > SizeOf(LBuffer) then
    raise Exception.Create(_('SafeGetSystemDirectory: buffer too small.'));
  Result := LBuffer;
end;

function RemoveTrailingPathDelimiter(const APath: string): string;
begin
  Result := APath;
  if (Result <> '') and (Result[Length(Result)] = PathDelim) then
    Delete(Result, Length(Result), 1);
end;

function GetTempDirectory: string;
begin
  SetLength(Result, MAX_PATH + 1);
  SetLength(Result, GetTempPath(MAX_PATH + 1, (PChar(Result))));
  Result := IncludeTrailingPathDelimiter(Result);
end;

function GetUniqueFileName(const APath, AExtension: string): string;
begin
  repeat
    Result := APath + GetRandomString(8) + AExtension;
  until not FileExists(Result);
end;

function GetTempFileName(const AFileExtension: string = '.tmp'): string;
begin
  Result := GetUniqueFileName(GetTempDirectory(), AFileExtension);
end;

function ExtractFileFormat(const AFileName: string): string;
begin
  Result := ExtractFileExt(AFileName);
  if (Result <> '') and (Result[1] = '.') then
    Delete(Result, 1, 1);
end;

function IsAbsolutePath(const APath: string): Boolean;
begin
  Result := (Pos(':', APath) <> 0) or (Pos('\\', APath) = 1) or (Pos('/', APath) = 1);
end;

procedure FindAllFiles(const AFileFormat: string; const ARootPath: string;
  const AFileNames: TStrings; const AIncludeSubFolders: Boolean = True;
  const AFullPaths: Boolean = True); overload;
begin
  FindAllFiles([AFileFormat], ARootPath, AFileNames, AIncludeSubFolders, AFullPaths);
end;

procedure FindAllFiles(const AFileFormats: array of string; const ARootPath: string;
  const AFileNames: TStrings; const AIncludeSubFolders: Boolean = True;
  const AFullPaths: Boolean = True);
var
  LFileFormats: TStringDynArray;
  LFileFormatIndex: Integer;
begin
  SetLength(LFileFormats, Length(AFileFormats));
  for LFileFormatIndex := 0 to Length(AFileFormats) - 1 do
    LFileFormats[LFileFormatIndex] := AFileFormats[LFileFormatIndex];
  with TEFFileLister.Create do
  begin
    try
      SourcePath := ARootPath;
      RecurseSubdirs := AIncludeSubFolders;
      ListFiles(AFileNames, LFileFormats, AFullPaths);
    finally
      Free;
    end;
  end;
end;

procedure EnumDirectories(const ARootPath: string; const AProc: TDirectoryProc);
var
  LSearchRec: TSearchRec;
  LResult: Integer;
begin
  Assert(Assigned(AProc));
  Assert(DirectoryExists(ARootPath));

  LResult := FindFirst(IncludeTrailingPathDelimiter(ARootPath) + '*.*', faDirectory, LSearchRec);
  while LResult = 0 do
  begin
    if ((LSearchRec.Attr and faDirectory <> 0) and (LSearchRec.Name <> '.') and (LSearchRec.Name <> '..')) then
      AProc(LSearchRec.Name);
    LResult := FindNext(LSearchRec);
  end;
  FindClose(LSearchRec);
end;

function IsDirectoryEmpty(const APath: string): Boolean;
var
  LSearchRec: TSearchRec;
begin
  Result := False;
  if DirectoryExists(APath) then
  begin
    try
      Result := (FindFirst(IncludeTrailingPathDelimiter(APath) + '*.*', faAnyFile, LSearchRec) = 0)
        and (FindNext(LSearchRec) = 0)
        and (FindNext(LSearchRec) <> 0);
    finally
      FindClose(LSearchRec);
    end;
  end;
end;

function GetCmdLineParamValue(const AParamName: string; const ADefaultValue: string = ''): string;
var
  LParamIndex: Integer;
begin
  Result := ADefaultValue;
  // Skip the last parameter, because it could never have one after itself.
  for LParamIndex := 1 to ParamCount - 1 do
  begin
    if SameText(ParamStr(LParamIndex), '/' + AParamName)
      or SameText(ParamStr(LParamIndex), '-' + AParamName) then
    begin
      Result := ParamStr(Succ(LParamIndex));
      Break;
    end;
  end;
end;

function GetProgramFilesx86Directory: string;
begin
  Result := GetEnvironmentVariable('ProgramFiles(x86)');
  if (Result = '') or not DirectoryExists(Result) then
    Result := GetProgramFilesDirectory;
end;

function GetProgramFilesDirectory: string;
begin
  Result := GetEnvironmentVariable('ProgramFiles');
  if (Result = '') or not DirectoryExists(Result) then
    raise Exception.Create('Could not find Program Files directory.');
end;

procedure DeleteFile(const AFileName: string);
begin
  with TEFFileDeleter.Create do
  begin
    try
      DeleteFile(AFileName);
    finally
      Free;
    end;
  end;
end;

procedure DeleteAllFiles(const APath: string);
begin
  with TEFFileDeleter.Create do
  begin
    try
      SourcePath := APath;
      DeleteEmptySubfolders := True;
      DeleteEmptyRootFolder := False;
      DeleteFiles;
    finally
      Free;
    end;
  end;
end;

procedure DeleteTree(const APath: string);
begin
  with TEFFileDeleter.Create do
  begin
    try
      SourcePath := APath;
      DeleteEmptySubfolders := True;
      DeleteEmptyRootFolder := True;
      DeleteFiles;
    finally
      Free;
    end;
  end;
end;

procedure CopyFile(const ASourceFileName, ADestinationFileName: string);
begin
  with TEFFileCopier.Create do
  begin
    try
      CopyFile( ASourceFileName, ADestinationFileName);
    finally
      Free;
    end;
  end;
end;

procedure CopyAllFilesAndFolders(const ASourcePath, ADestinationPath: string;
  const ABeforeEachFile: TEFBeforeProcessFileProc;
  const AAfterEachFile: TEFProcessFileProc);
begin
  with TEFFileCopier.Create do
  begin
    try
      BeforeProcessFile := ABeforeEachFile;
      AfterProcessFile := AAfterEachFile;
      SourcePath := ASourcePath;
      DestinationPath := ADestinationPath;
      CopyFiles;
    finally
      Free;
    end;
  end;
end;

procedure CopyAllFilesAndFoldersExcept(
  const ASourcePath, ADestinationPath: string;
  const AExceptions: array of string;
  const ABeforeEachFile: TEFBeforeProcessFileProc = nil;
  const AAfterEachFile: TEFProcessFileProc = nil);
var
  LExceptionIndex: Integer;
begin
  with TEFFileCopier.Create do
  begin
    try
      BeforeProcessFile := ABeforeEachFile;
      AfterProcessFile := AAfterEachFile;
      SourcePath := ASourcePath;
      DestinationPath := ADestinationPath;
      for LExceptionIndex := Low(AExceptions) to High(AExceptions) do
        Exceptions.Add(AExceptions[LexceptionIndex]);
      CopyFiles;
    finally
      Free;
    end;
  end;
end;

procedure CheckFileInUse(const AFileName: string);
begin
  with TEFFileChecker.Create do
  begin
    try
      CheckFile(AFileName);
    finally
      Free;
    end;
  end;
end;

function GetFormatSettings: TFormatSettings;
begin
  {$IFDEF D15+}
  Result := TFormatSettings.Create;
  {$ELSE}
  GetLocaleFormatSettings(GetThreadLocale, Result);
  {$ENDIF}
end;

{ TFileProcessor }

procedure TEFFileProcessor.CheckPreconditions;
begin
  if FSourcePath = '' then
    raise Exception.CreateFmt(_('%s: SourcePath not set.'), [ClassName]);
end;

constructor TEFFileProcessor.Create;
begin
  inherited Create;
  FRecurseSubdirs := DEFAULT_RECURSE_SUBDIRS;
  FFileMask := DEFAULT_FILE_MASK;
  FRetryCount := DEFAULT_RETRY_COUNT;
  FRetryDelay := DEFAULT_RETRY_DELAY;
  FDefaultErrorAction := DEFAULT_DEFAULT_ERROR_ACTION;
  FExceptions := TStringList.Create;
end;

procedure TEFFileProcessor.Process;
begin
  DoProcess(FSourcePath, FDestinationPath);
end;

destructor TEFFileProcessor.Destroy;
begin
  FreeAndNil(FExceptions);
  inherited;
end;

procedure TEFFileProcessor.DoProcess(const ASourcePath, ADestinationPath: string);
var
  LFileHandle: THandle;
  LFileData: TWin32FindData;
  LSourceFileSpec: string;
  LSourcePath: string;
  LDestinationPath: string;

  function IsSpecialDirectory(const AName: string): Boolean;
  begin
    Result := (AName = '.') or (AName = '..');
  end;

  function IsDirectory: Boolean;
  begin
    Result := ((LFileData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) = FILE_ATTRIBUTE_DIRECTORY);
  end;

  function IsException: Boolean;
  var
    LExceptionIndex: Integer;
  begin
    Result := False;
    for LExceptionIndex := 0 to FExceptions.Count - 1 do
      if StrMatches(LFileData.cFileName, FExceptions[LExceptionIndex]) then
      begin
        Result := True;
        Break;
      end;      
  end;

  // Called for every file item.
  procedure ProcessFileData;
  begin
    if not IsSpecialDirectory(LFileData.cFileName) and not IsDirectory and not IsException then
      ProcessFile(LSourcePath + LFileData.cFileName, LDestinationPath + LFileData.cFileName);
  end;

  // Called for every directory item.
  procedure ProcessDirectoryData;
  begin
    if not IsSpecialDirectory(LFileData.cFileName) and IsDirectory and not IsException then
      DoProcess(LSourcePath + LFileData.cFileName,
        LDestinationPath + LFileData.cFileName);
  end;

begin
  CheckPreconditions;
  LSourcePath := IncludeTrailingPathDelimiter(ASourcePath);
  LDestinationPath := IncludeTrailingPathDelimiter(ADestinationPath);
  ForceDirectories(LDestinationPath);
  // First round: get all matching files in the root folder. No subdirectories.
  LSourceFileSpec := LSourcePath + FFileMask;
  LFileHandle := FindFirstFile(PChar(LSourceFileSpec), LFileData);
  try
    if LFileHandle <> INVALID_HANDLE_VALUE then
    begin
      ProcessFileData;
      while FindNextFile(LFileHandle, LFileData) do
        ProcessFileData;
    end;
  finally
    Windows.FindClose(LFileHandle);
  end;
  // Second round: all directories, recursively.
  if FRecurseSubdirs then
  begin
    LSourceFileSpec := LSourcePath + '*.*';
    LFileHandle := FindFirstFile(PChar(LSourceFileSpec), LFileData);
    try
      if LFileHandle <> INVALID_HANDLE_VALUE then
      begin
        ProcessDirectoryData;
        while FindNextFile(LFileHandle, LFileData) do
          ProcessDirectoryData;
      end;
    finally
      Windows.FindClose(LFileHandle);
    end;
  end;
  AfterProcessDirectory(RemoveTrailingPathDelimiter(ASourcePath));
end;

function TEFFileProcessor.DoProcessFileError(const E: Exception;
  const ASourceFileName, ADestinationFileName: string): TEFFileErrorAction;
begin
  Result := FDefaultErrorAction;
  if Assigned(FOnProcessFileError) then
    FOnProcessFileError(ASourceFileName, ADestinationFileName, E, Result);
end;

procedure TEFFileProcessor.ProcessFile(const ASourceFileName, ADestinationFileName: string);
var
  LRetries: Integer;
  LRetryDelay: Integer;
  LAllow: Boolean;
  LDestinationFileName: string;
begin
  LRetries := FRetryCount;
  LRetryDelay := FRetryDelay;
  while True do
  begin
    try
      LAllow := True;
      LDestinationFileName := ADestinationFileName;
      if Assigned(FBeforeProcessFile) then
        FBeforeProcessFile(ASourceFileName, LDestinationFileName, LAllow);
      if LAllow then
      begin
        DoProcessFile(ASourceFileName, LDestinationFileName);
        if Assigned(FAfterProcessFile) then
          FAfterProcessFile(ASourceFileName, LDestinationFileName);
      end;
      Break;
    except
      on E: Exception do
      begin
        Dec(LRetries);
        if LRetries <= 0 then
        begin
          case DoProcessFileError(E, ASourceFileName, ADestinationFileName) of
            eaRetry:
            begin
              Inc(LRetries);
              Continue;
            end;
            eaSkip:
              Exit;
            eaAbort:
              Abort;
            eaFail:
              raise;
          end;
        end
        else
        begin
          if LRetryDelay > 0 then
            Sleep(LRetryDelay);
          Continue;
        end;
      end;
    end;
  end;
end;

procedure TEFFileProcessor.SetExceptions(const AValue: TStrings);
begin
  FExceptions.Assign(AValue);
end;

procedure TEFFileProcessor.AfterProcessDirectory(const ADirectoryName: string);
begin
end;

{ TEFFileDeleter }

procedure TEFFileDeleter.AfterProcessDirectory(const ADirectoryName: string);
begin
  inherited;
  if ADirectoryName = RemoveTrailingPathDelimiter(SourcePath) then
  begin
    if FDeleteEmptyRootFolder then
      DeleteDirectory(ADirectoryName);
  end
  else if FDeleteEmptySubfolders then
    DeleteDirectory(ADirectoryName);
end;

procedure TEFFileDeleter.DeleteDirectory(const ADirectoryName: string);
var
  lRetries: Integer;
  lRetryDelay: Integer;
begin
  lRetries := FRetryCount;
  lRetryDelay := FRetryDelay;
  while True do
  begin
    try
      if Assigned(FBeforeDeleteDirectory) then
        FBeforeDeleteDirectory(Self, ADirectoryName);
      DoDeleteDirectory(ADirectoryName);
      if Assigned(FAfterDeleteDirectory) then
        FAfterDeleteDirectory(Self, ADirectoryName);
      Break;
    except
      on E: Exception do
      begin
        Dec(lRetries);
        if lRetries <= 0 then
        begin
          case DoProcessFileError(E, ADirectoryName, '') of
            eaRetry:
            begin
              Inc(lRetries);
              Continue;
            end;
            eaSkip:
              Exit;
            eaAbort:
              Abort;
            eaFail:
              raise;
          end;
        end
        else
        begin
          if lRetryDelay > 0 then
            Sleep(lRetryDelay);
          Continue;
        end;
      end;
    end;
  end;
end;

procedure TEFFileDeleter.DeleteFile(const AFileName: string);
begin
  if FileExists(AFileName) then
    ProcessFile(AFileName, '');
end;

procedure TEFFileDeleter.DeleteFiles;
begin
  Process;
end;

procedure TEFFileDeleter.DoDeleteDirectory(const ADirectoryName: string);
begin
  if not RemoveDir(ADirectoryName) then
    raise Exception.CreateFmt(_('Error while removing folder "%s". Perhaps the folder is not empty or is in use.'),
      [ADirectoryName]);
end;

procedure TEFFileDeleter.DoProcessFile(const ASourceFileName, ADestinationFileName: string);
var
  ErrorMsg: string;
  LastError: Integer;
begin
  FileSetReadOnly(ASourceFileName, False);
  if not Windows.DeleteFile(PChar(ASourceFileName)) then
  begin
    LastError := GetLastError();
    if LastError <> 0 then
      ErrorMsg := Format(SOSError, [LastError, SysErrorMessage(LastError)])
    else
      ErrorMsg := SUnkOSError;
    raise Exception.CreateFmt(_('Error "%s" while removing file "%s". The file might be in use.'),
      [ErrorMsg, ASourceFileName]);
  end;
end;

{ TEFFileChecker }

procedure TEFFileChecker.CheckFile(const AFileName: string);
begin
  if FileExists(AFileName) then
    ProcessFile(AFileName, '');
end;

procedure TEFFileChecker.CheckFiles;
begin
  Process;
end;

procedure TEFFileChecker.DoProcessFile(const ASourceFileName, ADestinationFileName: string);
begin
  FileSetReadOnly(ASourceFileName, False);
  if IsFileInUse(ASourceFileName) then
    raise Exception.CreateFmt(_('The file "%s" might be in use.'), [ASourceFileName]);
end;

{ TEFFileWriter }

procedure TEFFileWriter.DoProcessFile(const ASourceFileName,
  ADestinationFileName: string);
var
  LErrorMsg: string;
  LastError: Integer;
begin
  if FileExists(ASourceFileName) then
  begin
    FileSetReadOnly(ASourceFileName, False);
    if not Windows.DeleteFile(PChar(ASourceFileName)) then
    begin
      LastError := GetLastError();
      if LastError <> 0 then
        LErrorMsg := Format(SOSError, [LastError, SysErrorMessage(LastError)])
      else
        LErrorMsg := SUnkOSError;
      raise Exception.CreateFmt(_('Error "%s" while removing file "%s". The file might be in use.'),
        [LErrorMsg, ASourceFileName]);
    end;
  end;
  with TFileStream.Create(ASourceFileName, fmCreate or fmShareDenyNone) do
  begin
    try
      Write(FFileContent[1], Length(FFileContent));
    finally
      Free;
    end;
  end;
end;

procedure TEFFileWriter.WriteFile(const AFileName, AFileContent: string);
begin
  FFileContent := AFileContent;
  try
    ProcessFile(AFileName, '');
  finally
    FFileContent := '';
  end;
end;

{ TSourceDestPathFileProcessor }

procedure TEFSourceDestPathFileProcessor.CheckPreconditions;
begin
  inherited;
  if FDestinationPath = '' then
    raise Exception.CreateFmt(_('%s: DestinationPath not specified.'), [ClassName]);
end;

{ TFileCopier }

procedure TEFFileCopier.CopyFile(const ASourceFileName, ADestinationFileName: string);
begin
  if FileExists(ASourceFileName) then
    ProcessFile(ASourceFileName, ADestinationFileName);
end;

procedure TEFFileCopier.CopyFiles;
begin
  Process;
end;

procedure TEFFileCopier.DoProcessFile(const ASourceFileName, ADestinationFileName: string);
var
  LastError: Integer;
  LErrorMsg: string;
begin
  if FileExists(ADestinationFileName) then
  begin
    FileSetReadOnly(ADestinationFileName, False);
  end
  else
    ForceDirectories(ExtractFilePath(ADestinationFileName));
  // Recupero del messaggio di errore corretto per sollevare l'eccezione.
  if not Windows.CopyFile(PChar(ASourceFileName), PChar(ADestinationFileName), False) then
  begin
    LastError := GetLastError();
    if LastError <> 0 then
      LErrorMsg := Format(SOSError, [LastError, SysErrorMessage(LastError)])
    else
      LErrorMsg := SUnkOSError;
    raise Exception.CreateFmt(_('Error "%s" while copying from "%s" to "%s". The destination file might be in use.'),
      [LErrorMsg, ASourceFileName, ADestinationFileName]);
  end;
end;

{ TEFFileLister }

procedure TEFFileLister.DoProcessFile(const ASourceFileName,
  ADestinationFileName: string);

  function IsFileFormatAllowed(const AFileFormat: string): Boolean;
  var
    LFileFormatIndex: Integer;
  begin
    Result := False;
    for LFileFormatIndex := Low(FFileFormats) to High(FFileFormats) do
    begin
      if SameText(FFileFormats[LFileFormatIndex], '*') or
        SameText(FFileFormats[LFileFormatIndex], AFileFormat) then
      begin
        Result := True;
        Break;
      end;
    end;
  end;

begin
  if IsFileFormatAllowed(ExtractFileFormat(ASourceFileName)) then
  begin
    if FFullPaths then
      FFileNameList.Add(ASourceFileName)
    else
      FFileNameList.Add(ExtractFileName(ASourceFileName));
  end;
end;

function TEFFileLister.ListFiles(const AFileNameList: TStrings;
  const AFileFormats: TStringDynArray; const AFullPaths: Boolean = True): Integer;
var
  LInitialItemCount: Integer;
begin
  Assert(Assigned(AFileNameList));
  Assert(Length(AFileFormats) > 0);

  FFileNameList := AFileNameList;
  FFileFormats := AFileFormats;
  FFullPaths := AFullPaths;
  LInitialItemCount := FFileNameList.Count;
  Process;
  Result := FFileNameList.Count - LInitialItemCount;
end;

function SameDirectory(const ADirectory1, ADirectory2: string): Boolean;
var
  LDirectory1, LDirectory2: string;
  LOldCurrentDir: string;
begin
  LOldCurrentDir := GetCurrentDir;
  try
    ChDir(ADirectory1);
    LDirectory1 := GetCurrentDir;
    ChDir(ADirectory2);
    LDirectory2 := GetCurrentDir;

    Result := SameText(LDirectory1, LDirectory2);
  finally
    SetCurrentDir(LOldCurrentDir);
  end;
end;

function GetCurrentProcessMemory: Cardinal;
var
  LCounters: TProcessMemoryCounters;
begin
  FillChar(LCounters, SizeOf(LCounters), 0);
  LCounters.cb := SizeOf(LCounters);
  if GetProcessMemoryInfo(GetCurrentProcess, @LCounters, SizeOf(LCounters)) then
    Result := LCounters.WorkingSetSize
  else
    Result := 0;
end;

procedure ShrinkProcessWorkingSet(const AMaxMemory: Cardinal);
begin
  if GetCurrentProcessMemory > AMaxMemory then
    EmptyWorkingSet(GetCurrentProcess);
end;

function GetDataType(const ABytes: TBytes; const ADefault: string): string;
const
  MIN_BYTES = 8;
begin
  Assert(Assigned(ABytes));

  Result := ADefault;

  if Length(ABytes) >= MIN_BYTES then
  begin
    if (ABytes[0] = 66) and (ABytes[1] = 77) then
      Result := 'bmp'
    else if ((ABytes[0] = 73) and (ABytes[1] = 73) and (ABytes[2] = 42) and (ABytes[3] = 0))
        or ((ABytes[0] = 77) and (ABytes[1] = 77) and (ABytes[2] = 42) and (ABytes[3] = 0)) then
      Result := 'tif'
    else if (ABytes[0] = $FF) and (ABytes[1] = $D8) then
      Result := 'jpg'
    else if (ABytes[0] = $89) and (ABytes[1] = $50) and (ABytes[2] = $4E) and (ABytes[3] = $47)
        and (ABytes[4] = $0D) and (ABytes[5] = $0A) and (ABytes[6] = $1A) and (ABytes[7] = $0A) then
      Result := 'png'
    else if (ABytes[0] = 177) and (ABytes[1] = 104) and (ABytes[2] = 222) and (ABytes[3] = 58) then
      Result := 'dcx'
    else if ABytes[0] = 10 then
      Result := 'pcx'
    else if ((ABytes[0] = 215) and (ABytes[1] = 205) and (ABytes[2] = 198) and (ABytes[3] = 154))
        or ((ABytes[0] = 1) and (ABytes[1] = 0) and (ABytes[2] = 0) and (ABytes[3] = 0)) then
      Result := 'emf'
    else if (ABytes[0] = $47) and (ABytes[1] = $49) and (ABytes[2] = $46) then
      Result := 'gif'
    else if (ABytes[0] = 0) and (ABytes[1] = 0) and (ABytes[2] = 1) and (ABytes[3] = 0) then
      Result := 'ico'
    else if (ABytes[0] = $25) and (ABytes[1] = $50) and (ABytes[2] = $44) and (ABytes[3] = $46) then
      Result := 'pdf';
  end;
end;

end.
