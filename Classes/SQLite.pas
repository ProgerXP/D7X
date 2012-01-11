unit SQLite;

{
  OOP SQLite interface for Delphi 7 for SQLite 3.
  in public domain | by Proger_XP | http://proger.i-forge.net/SQLite_for_Delphi_7

  Initially based on the sqlite unit from wellwell at http://www.rsdn.ru/forum/delphi/2480456.all.aspx.

  SQLite downloads:           http://www.sqlite.org/download.html
  SQLite 3 C API reference:   http://www.sqlite.org/c3ref/funclist.html
}

interface

uses Classes, Contnrs, SysUtils, Windows, StringsW, FileStreamW, Utils;

const
  { Result codes: }
  SQLITE_OK = $00;
  SQLITE_ERROR = $01;
  SQLITE_INTERNAL = $02;
  SQLITE_PERM = $03;
  SQLITE_ABORT = $04;
  SQLITE_BUSY = $05;
  SQLITE_LOCKED = $06;
  SQLITE_NOMEM = $07;
  SQLITE_READONLY = $08;
  SQLITE_INTERRUPT = $09;
  SQLITE_IOERR = $0A;
  SQLITE_CORRUPT = $0B;
  SQLITE_NOTFOUND = $0C;
  SQLITE_FULL = $0D;
  SQLITE_CANTOPEN = $0E;
  SQLITE_PROTOCOL = $0F;
  SQLITE_EMPTY = $10;
  SQLITE_SCHEMA = $11;
  SQLITE_TOOBIG = $12;
  SQLITE_CONSTRAINT = $13;
  SQLITE_MISMATCH = $14;
  SQLITE_MISUSE = $15;
  SQLITE_NOLFS = $16;
  SQLITE_AUTH = $17;
  SQLITE_FORMAT = $18;
  SQLITE_RANGE = $19;
  SQLITE_NOTADB = $1A;
  SQLITE_ROW = $64;
  SQLITE_DONE = $65;

  { Error codes: }
  SQLITE_OPEN_READONLY           = $00000001;  { Ok for sqlite3_open_v2() }
  SQLITE_OPEN_READWRITE          = $00000002;  { Ok for sqlite3_open_v2() }
  SQLITE_OPEN_CREATE             = $00000004;  { Ok for sqlite3_open_v2() }
  SQLITE_OPEN_DELETEONCLOSE      = $00000008;  { VFS only }
  SQLITE_OPEN_EXCLUSIVE          = $00000010;  { VFS only }
  SQLITE_OPEN_AUTOPROXY          = $00000020;  { VFS only }
  SQLITE_OPEN_URI                = $00000040;  { Ok for sqlite3_open_v2() }
  SQLITE_OPEN_MAIN_DB            = $00000100;  { VFS only }
  SQLITE_OPEN_TEMP_DB            = $00000200;  { VFS only }
  SQLITE_OPEN_TRANSIENT_DB       = $00000400;  { VFS only }
  SQLITE_OPEN_MAIN_JOURNAL       = $00000800;  { VFS only }
  SQLITE_OPEN_TEMP_JOURNAL       = $00001000;  { VFS only }
  SQLITE_OPEN_SUBJOURNAL         = $00002000;  { VFS only }
  SQLITE_OPEN_MASTER_JOURNAL     = $00004000;  { VFS only }
  SQLITE_OPEN_NOMUTEX            = $00008000;  { Ok for sqlite3_open_v2() }
  SQLITE_OPEN_FULLMUTEX          = $00010000;  { Ok for sqlite3_open_v2() }
  SQLITE_OPEN_SHAREDCACHE        = $00020000;  { Ok for sqlite3_open_v2() }
  SQLITE_OPEN_PRIVATECACHE       = $00040000;  { Ok for sqlite3_open_v2() }
  SQLITE_OPEN_WAL                = $00080000;  { VFS only }

  { Extended error codes since 3.3.8: }
  SQLITE_IOERR_READ              = SQLITE_IOERR    or (1 shl 8);
  SQLITE_IOERR_SHORT_READ        = SQLITE_IOERR    or (2 shl 8);
  SQLITE_IOERR_WRITE             = SQLITE_IOERR    or (3 shl 8);
  SQLITE_IOERR_FSYNC             = SQLITE_IOERR    or (4 shl 8);
  SQLITE_IOERR_DIR_FSYNC         = SQLITE_IOERR    or (5 shl 8);
  SQLITE_IOERR_TRUNCATE          = SQLITE_IOERR    or (6 shl 8);
  SQLITE_IOERR_FSTAT             = SQLITE_IOERR    or (7 shl 8);
  SQLITE_IOERR_UNLOCK            = SQLITE_IOERR    or (8 shl 8);
  SQLITE_IOERR_RDLOCK            = SQLITE_IOERR    or (9 shl 8);
  SQLITE_IOERR_DELETE            = SQLITE_IOERR    or (10 shl 8);
  SQLITE_IOERR_BLOCKED           = SQLITE_IOERR    or (11 shl 8);
  SQLITE_IOERR_NOMEM             = SQLITE_IOERR    or (12 shl 8);
  SQLITE_IOERR_ACCESS            = SQLITE_IOERR    or (13 shl 8);
  SQLITE_IOERR_CHECKRESERVEDLOCK = SQLITE_IOERR    or (14 shl 8);
  SQLITE_IOERR_LOCK              = SQLITE_IOERR    or (15 shl 8);
  SQLITE_IOERR_CLOSE             = SQLITE_IOERR    or (16 shl 8);
  SQLITE_IOERR_DIR_CLOSE         = SQLITE_IOERR    or (17 shl 8);
  SQLITE_IOERR_SHMOPEN           = SQLITE_IOERR    or (18 shl 8);
  SQLITE_IOERR_SHMSIZE           = SQLITE_IOERR    or (19 shl 8);
  SQLITE_IOERR_SHMLOCK           = SQLITE_IOERR    or (20 shl 8);
  SQLITE_IOERR_SHMMAP            = SQLITE_IOERR    or (21 shl 8);
  SQLITE_IOERR_SEEK              = SQLITE_IOERR    or (22 shl 8);
  SQLITE_LOCKED_SHAREDCACHE      = SQLITE_LOCKED   or  (1 shl 8);
  SQLITE_BUSY_RECOVERY           = SQLITE_BUSY     or  (1 shl 8);
  SQLITE_CANTOPEN_NOTEMPDIR      = SQLITE_CANTOPEN or (1 shl 8);
  SQLITE_CORRUPT_VTAB            = SQLITE_CORRUPT  or (1 shl 8);
  SQLITE_READONLY_RECOVERY       = SQLITE_READONLY or (1 shl 8);
  SQLITE_READONLY_CANTLOCK       = SQLITE_READONLY or (2 shl 8);

  { Data types: }
  SQLITE_INTEGER                 = 1;
  SQLITE_FLOAT                   = 2;
  SQLITE_TEXT                    = 3;
  SQLITE_BLOB                    = 4;
  SQLITE_NULL                    = 5;

  { String types: }
  SQLITE_UTF8 = $01;
  SQLITE_UTF16LE = $02;
  SQLITE_UTF16BE = $03;
  SQLITE_UTF16 = $04;
  SQLITE_ANY = $05;

  { sqlite3_set_authorizer()                  3rd              4th          }
  SQLITE_CREATE_INDEX             =  1;   { Index Name      Table Name      }
  SQLITE_CREATE_TABLE             =  2;   { Table Name      NULL            }
  SQLITE_CREATE_TEMP_INDEX        =  3;   { Index Name      Table Name      }
  SQLITE_CREATE_TEMP_TABLE        =  4;   { Table Name      NULL            }
  SQLITE_CREATE_TEMP_TRIGGER      =  5;   { Trigger Name    Table Name      }
  SQLITE_CREATE_TEMP_VIEW         =  6;   { View Name       NULL            }
  SQLITE_CREATE_TRIGGER           =  7;   { Trigger Name    Table Name      }
  SQLITE_CREATE_VIEW              =  8;   { View Name       NULL            }
  SQLITE_DELETE                   =  9;   { Table Name      NULL            }
  SQLITE_DROP_INDEX               = 10;   { Index Name      Table Name      }
  SQLITE_DROP_TABLE               = 11;   { Table Name      NULL            }
  SQLITE_DROP_TEMP_INDEX          = 12;   { Index Name      Table Name      }
  SQLITE_DROP_TEMP_TABLE          = 13;   { Table Name      NULL            }
  SQLITE_DROP_TEMP_TRIGGER        = 14;   { Trigger Name    Table Name      }
  SQLITE_DROP_TEMP_VIEW           = 15;   { View Name       NULL            }
  SQLITE_DROP_TRIGGER             = 16;   { Trigger Name    Table Name      }
  SQLITE_DROP_VIEW                = 17;   { View Name       NULL            }
  SQLITE_INSERT                   = 18;   { Table Name      NULL            }
  SQLITE_PRAGMA                   = 19;   { Pragma Name     1st arg or NULL }
  SQLITE_READ                     = 20;   { Table Name      Column Name     }
  SQLITE_SELECT                   = 21;   { NULL            NULL            }
  SQLITE_TRANSACTION              = 22;   { Operation       NULL            }
  SQLITE_UPDATE                   = 23;   { Table Name      Column Name     }
  SQLITE_ATTACH                   = 24;   { Filename        NULL            }
  SQLITE_DETACH                   = 25;   { Database Name   NULL            }
  SQLITE_ALTER_TABLE              = 26;   { Database Name   Table Name      }
  SQLITE_REINDEX                  = 27;   { Index Name      NULL            }
  SQLITE_ANALYZE                  = 28;   { Table Name      NULL            }
  SQLITE_CREATE_VTABLE            = 29;   { Table Name      Module Name     }
  SQLITE_DROP_VTABLE              = 30;   { Table Name      Module Name     }
  SQLITE_FUNCTION                 = 31;   { NULL            Function Name   }
  SQLITE_SAVEPOINT                = 32;   { Operation       Savepoint Name  }
  SQLITE_COPY                     =  0;   { No longer used }

  { For sqlite3_status(): }
  SQLITE_STATUS_MEMORY_USED            = 0;
  SQLITE_STATUS_PAGECACHE_USED         = 1;
  SQLITE_STATUS_PAGECACHE_OVERFLOW     = 2;
  SQLITE_STATUS_SCRATCH_USED           = 3;
  SQLITE_STATUS_SCRATCH_OVERFLOW       = 4;
  SQLITE_STATUS_MALLOC_SIZE            = 5;
  SQLITE_STATUS_PARSER_STACK           = 6;
  SQLITE_STATUS_PAGECACHE_SIZE         = 7;
  SQLITE_STATUS_SCRATCH_SIZE           = 8;
  SQLITE_STATUS_MALLOC_COUNT           = 9;

  { For sqlite3_db_status(): }
  SQLITE_DBSTATUS_LOOKASIDE_USED       = 0;
  SQLITE_DBSTATUS_CACHE_USED           = 1;
  SQLITE_DBSTATUS_SCHEMA_USED          = 2;
  SQLITE_DBSTATUS_STMT_USED            = 3;
  SQLITE_DBSTATUS_LOOKASIDE_HIT        = 4;
  SQLITE_DBSTATUS_LOOKASIDE_MISS_SIZE  = 5;
  SQLITE_DBSTATUS_LOOKASIDE_MISS_FULL  = 6;
  SQLITE_DBSTATUS_CACHE_HIT            = 7;
  SQLITE_DBSTATUS_CACHE_MISS           = 8;
  SQLITE_DBSTATUS_MAX                  = 8;   { Largest defined DBSTATUS }

  { For sqlite3_limit(): }
  SQLITE_LIMIT_LENGTH                    = 0;
  SQLITE_LIMIT_SQL_LENGTH                = 1;
  SQLITE_LIMIT_COLUMN                    = 2;
  SQLITE_LIMIT_EXPR_DEPTH                = 3;
  SQLITE_LIMIT_COMPOUND_SELECT           = 4;
  SQLITE_LIMIT_VDBE_OP                   = 5;
  SQLITE_LIMIT_FUNCTION_ARG              = 6;
  SQLITE_LIMIT_ATTACHED                  = 7;
  SQLITE_LIMIT_LIKE_PATTERN_LENGTH       = 8;
  SQLITE_LIMIT_VARIABLE_NUMBER           = 9;
  SQLITE_LIMIT_TRIGGER_DEPTH             = 10;

type
  PCharArray = array[$0..$1FFFFFFE] of PChar;
  PPCharArray = ^PCharArray;
  TVAList = array[0..$FFFF] of Pointer;

  TSQLiteType = (sqInteger, sqFloat, sqText, sqBlob, sqNull);
  TSQLiteAction = (sqInsert, sqDelete, sqUpdate);
  TSQLiteTransactionType = (trNormal, trDeferred, trImmediate, trExclusive);

  TSQLiteDatabase = class;
  TSQLiteTable = class;
  TSQLiteQuery = class;
  TSQLiteResult = class;
  TSQLiteBlob = class;

  TSQLiteBDestructor = procedure (Data: Pointer) cdecl;

  { Return True to cancel the operation. }
  TSQLiteProgressCallback = function (Data: Pointer): Boolean cdecl;
  TSQLiteProgressEvent = function (DB: TSQLiteDatabase): Boolean of object;

  { Return True to try again or False to terminate the operation with SQLITE_BUSY or SQLITE_IOERR_BLOCKED. }
  TSQLiteBusyCallback = function (Data: Pointer; TryCount: Integer): Boolean cdecl;
  TSQLiteBusyEvent = function (DB: TSQLiteDatabase; TryCount: Integer): Boolean of object;

  { Commit becomes a rollback if the function returns True. }
  TSQLiteCommitCallback = TSQLiteProgressCallback;
  TSQLiteCommitEvent = TSQLiteProgressEvent;

  TSQLiteRollbackCallback = procedure (Data: Pointer) cdecl;
  TSQLiteRollbackEvent = procedure (DB: TSQLiteDatabase) of object;

  TSQLiteUpdateCallback = procedure (Data: Pointer; Action: Integer; Database, Table: PChar; RowID: Int64) cdecl;
  TSQLiteUpdateEvent = procedure (DB: TSQLiteDatabase; Action: TSQLiteAction; Table: TSQLiteTable; RowID: Int64) of object;

  ESQLite = class (Exception)
  protected
    FMessageUpdated: Boolean;
  public
    Code: Integer;
    Msg: String;

    LastError, LastErrorExt: Integer;         // set by FillWithInfoFrom, if called
    LastErrorMsg: String;                     // set by FillWithInfoFrom, if called

    DbObject: TSQLiteDatabase;                // may be NIL

    constructor Create(Code: Integer; Msg: String = ''); overload;
    constructor Create(Code: Integer; Msg: PPChar); overload;

    procedure FillWithInfoFrom(DB: Pointer);  // fills LastError* props
  end;

    ESQLiteOpenDB = class (ESQLite)
    public
      FN, VFS: WideString;
      Flags: Integer;

      constructor Create(Code: Integer; const FN: WideString; Flags: Integer; const VFS: WideString);
    end;

    ESQLiteLoadingExtension = class (ESQLite)
    public
      FN, EntryPoint: WideString;
      constructor Create(Code: Integer; Msg: PPChar; const FN, EntryPoint: WideString);
    end;

    ESQLiteAllowingLoadExt = class (ESQLite)
    end;

    ESQLiteInvalidType = class (ESQLite)
    public
      Kind: Integer;
      constructor Create(Kind: Integer);
    end;

    ESQLiteUnsupportedPrintfArg = class (ESQLite)
    public
      VType: Integer;
      constructor Create(VType: Integer);
    end;

    ESQLiteInvalidUpdateAction = class (ESQLite)
    public
      Action: Integer;
      constructor Create(Action: Integer);
    end;

    ESQLiteBlob = class (ESQLite)
    end;

      ESQLiteOpenTableBlob = class (ESQLiteBlob)
      public
        DB, Table, Field: WideString;
        RowID: Int64;
        constructor Create(Code: Integer; const DB, Table, Field: WideString; RowID: Int64);
      end;

      ESQLiteSettingBlobSize = class (ESQLiteBlob)
      public
        OldSize, NewSize: Integer;
        constructor Create(OldSize, NewSize: Integer);
      end;

      ESQLiteBlobOperation = class (ESQLiteBlob)
      public
        Operation: String;
        constructor Create(Code: Integer; Operation: String);
      end;

    ESQLiteQuery = class (ESQLite)
    end;

      ESQLiteEmptySQL = class (ESQLiteQuery)
      public
        constructor Create;
      end;

      ESQLitePreparingSQL = class (ESQLiteQuery)
      public
        SQL: WideString;
        constructor Create(Code: Integer; const SQL: WideString);
      end;

        ESQLiteExecutingSQL = class (ESQLitePreparingSQL)
        end;

      ESQLiteSteppingSQL = class (ESQLiteQuery)
      end;

      ESQLiteNoColumnByIndex = class (ESQLiteQuery)
      public
        Index: Integer;
        constructor Create(Index: Integer);
      end;

      ESQLiteNoColumnByName = class (ESQLiteQuery)
      public
        Name, All: WideString;
        constructor Create(const Name, All: WideString);
      end;

      ESQLiteNoMoreResults = class (ESQLiteQuery)
      public
        Operation: WideString;
        constructor Create(const Operation: WideString);
      end;

      ESQLiteResettingResult = class (ESQLiteQuery)
      end;

      ESQLiteBind = class (ESQLiteQuery)
      public
        ParamIndex: Integer;
        ParamName: WideString;

        constructor Create(Code: Integer; Param: Integer); overload;
        constructor Create(Code: Integer; const Param: WideString); overload;
      end;

        ESQLiteEmptyBindParam = class (ESQLiteBind)
        public
          ParamName: WideString;
          constructor Create(const Param: WideString);
        end;

        ESQLiteUnsupportedBindArg = class (ESQLiteBind)
        public
          ParamName: WideString;
          ParamIndex, VType: Integer;

          constructor Create(Param: WideString; VType: Integer); overload;
          constructor Create(Param, VType: Integer); overload;
        end;

      ESQLiteResultToRecord = class (ESQLiteQuery)
      public
        constructor Create(Reason: WideString);
      end;

  TSQLiteColumn = record
    // Origin is an unaliased original column name.
    Name, Origin: WideString;
    // column declaration type: http://www.sqlite.org/c3ref/column_decltype.html
    DeclType: WideString;

    BLOB: String;
    Text: WideString;

    case Kind: TSQLiteType of
    sqInteger: (Int: Integer; Int64: Int64);
    sqFloat: (Float: Double);
  end;

  { Owning. }
  TSQLiteObjectList = class (TObjectList)
  public
    constructor Create;
    procedure Delete(ObjInst: TObject); overload;   // does nothing if ObjInst doesn't exist.
  end;

  TSQLiteObject = class (TObject);

  TSQLite = class
  public
    class function Version: WideString;
    class function VersionNum: Integer;
    class function SourceID: WideString;

    class function Quote(const Str: WideString): WideString;
    class function QuoteWrapping(const Str: WideString): WideString;
    // if Str = '' returns "NULL", otherwise works as QuoteWrapping.
    class function QuoteOrNull(const Str: WideString): WideString;

    class function IsThreadSafe: Boolean;
    class function HasCompileOption(const Name: WideString): Boolean;
    class function CompileOptions(WithPrefix: Boolean = False): TStrings;

    class function Random(Size: Integer): TMemoryStream;
    // attempts to free non-essential memory and returns the amount that was actually freed;
    // works only if SQLITE_ENABLE_MEMORY_MANAGEMENT was enabled on compile-time.
    class function ReleaseMemory(Amount: Integer): Integer;
    class function SoftHeapLimit(NewLimit: Int64 = -1): Int64;    // returns old limit.

    class function Error(Err: Integer): WideString;
    class function Printf(Format: WideString; const Args: array of const): WideString;

    class function MemoryPeak(Reset: Boolean = False): Int64;
    class function MemoryUsed: Int64;

    // Op - one of SQLITE_STATUS_* consts; returns SQLITE_OK on success or an error code
    // on failure (Current and Peak are set to 0).
    class function Status(Op: Integer; out Current, Peak: Integer; Reset: Boolean = False): Integer;
    class function StrIComp(const S1, S2: WideString): Integer;
  end;

  TSQLiteDatabase = class (TSQLiteObject)
  protected
    FOpenFileName, FOpenVFS: WideString;
    FOpenFlags: Integer;
    FDeleteOnClose: Boolean;

    FHandle: Pointer;
    FTables: TObjectHash;         // cached TSQLiteTable objects (tables don't have to exist)
    FQueries: TSQLiteObjectList;  // of TSQLiteQuery
    FTransactionNesting: Integer;

    FOnProgress: TSQLiteProgressEvent;
    FOnProgressOpCodeCount: Integer;
    FOnBusy: TSQLiteBusyEvent;
    FBusyTimeout: Integer;
    FOnCommit: TSQLiteCommitEvent;
    FOnRollback: TSQLiteRollbackEvent;
    FOnUpdate: TSQLiteUpdateEvent;

    constructor CreateCustom(Stream: TStream; Flags: Integer); overload;

    procedure Init;
    procedure PrepareConnection;
    procedure Close;

    function GetTable(Name: WideString): TSQLiteTable;
    function GetLimit(Op: Integer): Integer;
    procedure SetLimit(Op: Integer; Value: Integer);
    function GetForeignKeys: Boolean;
    procedure SetForeignKeys(const Value: Boolean);

    procedure SetOnProgress(Value: TSQLiteProgressEvent);
    procedure SetOnProgressOpCodeCount(const Value: Integer);
    procedure SetOnBusy(const Value: TSQLiteBusyEvent);
    procedure SetBusyTimeout(Value: Integer);
    procedure SetOnCommit(Value: TSQLiteCommitEvent);
    procedure SetOnRollback(Value: TSQLiteRollbackEvent);
    procedure SetOnUpdate(Value: TSQLiteUpdateEvent);
  public
    { delegates to TSQLite; useful to call on a created DB object and thus avoid
      references to TSQLite in code that might be database-abstracted. }
    class function Quote(const Str: WideString): WideString;
    class function QuoteWrapping(const Str: WideString): WideString;
    class function QuoteOrNull(const Str: WideString): WideString;

    class function GetTempFileName: WideString; virtual;

    constructor Create(const FN: WideString);   // opens FN if exists or creates it
    constructor New(const FN: WideString);      // fails if FN exists
    constructor CreateInMemory;
    constructor CreateTemp;

    { Stream methods load data from current position till the end and restore the pointer. }
    constructor Open(const FN: WideString); overload;
    constructor Open(Stream: TStream); overload;
    constructor OpenReadOnly(const FN: WideString); overload;
    constructor OpenReadOnly(Stream: TStream); overload;

    constructor CreateCustom(const FN: WideString; Flags: Integer; const VFS: WideString = ''); overload;
    constructor Acquire(Handle: Pointer);
    destructor Destroy; override;

    property FileName: WideString read FOpenFileName;
    property OpenVFS: WideString read FOpenVFS;
    property OpenFlags: Integer read FOpenFlags;
    property DeleteOnClose: Boolean read FDeleteOnClose write FDeleteOnClose;

    property Handle: Pointer read FHandle;
    // don't free table objects.
    property Tables[Name: WideString]: TSQLiteTable read GetTable; default;
    function TableNames: TStringsW;
    procedure TablesOf(DB: WideString; Dest: TStringsW);

    function LastInsertID: Int64;
    function LastChanged: Integer;
    procedure Interrupt;                  // cancels any pending database operations.
    function IsAutocommitting: Boolean;   // returns False if inside a transaction.
    // usually False by default; attempt to set ForeignKeys while in transaction causes an error.
    property ForeignKeys: Boolean read GetForeignKeys write SetForeignKeys;

    procedure BeginTransaction(Kind: TSQLiteTransactionType = trNormal);
    procedure Commit;
    procedure Rollback;
    { The following only work when transaction is operated through calls to 3 methods above
      instead of doing this directly through Execute or similar. }
    property TransactionNesting: Integer read FTransactionNesting;
    function InTransaction: Boolean;
    function BeginNestedTransaction(Kind: TSQLiteTransactionType = trNormal): Boolean;
    function CommitNested: Boolean;       // True if there was no nesting and commit was performed.

    function LastError: Integer;
    function LastErrorExt: Integer;       // one of extended error codes.
    function LastErrorMsg: WideString;

    // returned objects can be freed by caller using Free; others are freed automatically
    // when the database object is destroyed.
    function NewQuery(const SQL: WideString; Bind: array of const): TSQLiteQuery;
    // returns TSQLiteEmptyResult (NilIfNone = False) or NIL if there were no rows in the result set.
    function QueryResult(const SQL: WideString; Bind: array of const;
      NilIfNone: Boolean = False): TSQLiteResult;
    // the same as NewQuery but doesn't return an object; instead returns the sum of
    // LastChanged for all queries inside SQL (";"-separated).
    function Execute(const SQL: WideString; Bind: array of const): Integer;

    property OnProgress: TSQLiteProgressEvent read FOnProgress write SetOnProgress;
    property OnProgressOpCodeCount: Integer read FOnProgressOpCodeCount write SetOnProgressOpCodeCount default 30;
    // is called when a locked table is attempted to be accessed:
    property OnBusy: TSQLiteBusyEvent read FOnBusy write SetOnBusy;
    // overrides OnBusy handler; sets a period during which the DB will attempt to access
    // the locked resource before returning with an error.
    // Milliseconds; set to 0 or below to disable and use OnBusy (if it's assigned).
    property BusyTimeout: Integer read FBusyTimeout write SetBusyTimeout;
    property OnCommit: TSQLiteCommitEvent read FOnCommit write SetOnCommit;
    property OnRollback: TSQLiteRollbackEvent read FOnRollback write SetOnRollback;
    property OnUpdate: TSQLiteUpdateEvent read FOnUpdate write SetOnUpdate;

    // Op - one of SQLITE_DBSTATUS_* consts.
    function Status(Op: Integer; out Current, Peak: Integer; Reset: Boolean = False): Integer;
    // Op - one of SQLITE_LIMIT_* consts.
    property Limit[Op: Integer]: Integer read GetLimit write SetLimit;

    procedure AllowLoadingExtensions(Allow: Boolean);
    procedure LoadExtension(const FN: WideString; const EntryPoint: WideString = '');
  end;

  TSQLiteTable = class (TSQLiteObject)
  protected
    FDatabase: TSQLiteDatabase;
    FName: WideString;
    FBlobs: TSQLiteObjectList;

    constructor Create(DB: TSQLiteDatabase; const Name: WideString);
  public
    destructor Destroy; override;

    property Name: WIdeString read FName;
    function QueryCount(Where: WideString = ''): Int64; overload;
    function QueryCount(Where: WideString; Bind: array of const): Int64; overload;

    // DB can be 'main', 'temp' or a name of an ATTACH'ed database.
    function OpenBlob(Field: WideString; RowID: Int64; ReadOnly: Boolean = False;
      DB: WideString = 'main'): TSQLiteBlob;
  end;

  TSQLiteQuery = class (TSQLiteObject)
  protected
    FFullSQL, FNextSQL: WideString;
    FDatabase: TSQLiteDatabase;

    FHandle: Pointer;

    // query objects are created by TSQLiteDatabase.NewQuery.
    constructor Create(const SQL: WideString; DB: TSQLiteDatabase);

    // caller must sqlite3_finalize(Result) on its own after it's done with the statement.
    function PrepareNext: Pointer;
    function Execute(FailOnSqlError: Boolean = True): Integer;
  public
    destructor Destroy; override;

    property FullSQL: WideString read FFullSQL;
    property NextSQL: WideString read FNextSQL;

    // returns True if current query doesn't change the database directly: http://www.sqlite.org/c3ref/stmt_readonly.html
    function IsReadOnly: Boolean;
    // Op - one of SQLITE_STATUS_* consts.
    function Status(Op: Integer; Reset: Boolean = False): Integer;

    // might be useless since query is only ran by Run which passes the handler to
    // TSQLiteResult so its Reset can only be used.
    procedure Reset;
    procedure ClearBindings;

    // Bind = [ [<name,] <value>, [<name>,] <value> ]; <name> - String starting with ":" or "?";
    // if <value> has no leading <name> parameter index is used (total count of preceding <value>s)
    // if <name> starts with "?" an error is raised if value with given name doesn't exist
    // <value> can be Integer, Double/Extended, Char/WideString (BindTextTo) or String (BindBlobTo).
    procedure Bind(Values: array of const);
    procedure CheckBindRes(const Param: WideString; Status: Integer); overload;
    procedure CheckBindRes(Param: Integer; Status: Integer); overload;

      procedure BindDoubleTo(const Param: WideString; Value: Double); overload;
      procedure BindDoubleTo(Param: Integer; Value: Double); overload;
      procedure BindIntegerTo(const Param: WideString; Value: Int64); overload;
      procedure BindIntegerTo(Param: Integer; Value: Int64); overload;
      procedure BindNullTo(const Param: WideString); overload;
      procedure BindNullTo(Param: Integer); overload;
      procedure BindTextTo(const Param: WideString; const Value: WideString); overload;
      procedure BindTextTo(Param: Integer; const Value: WideString); overload;

      procedure BindBlobTo(const Param: WideString; const Value: String); overload;
      procedure BindBlobTo(Param: Integer; const Value: String); overload;
      procedure BindBlobTo(const Param: WideString; const Buf; Size: Integer); overload;
      procedure BindBlobTo(Param: Integer; const Buf; Size: Integer); overload;
      { Stream is read from current position to the end and old position is then restored. }
      procedure BindBlobTo(const Param: WideString; Stream: TStream); overload;
      procedure BindBlobTo(Param: Integer; Stream: TStream); overload;

      procedure BindZeroBlobTo(const Param: WideString; Size: Integer); overload;
      procedure BindZeroBlobTo(Param: Integer; Size: Integer); overload;

    // parameter indexes are 1-based in SQLite; 0 means "no such param".
    function ParamIndex(const Param: WideString): Integer;
    function ParamName(Index: Integer): WideString;   // returns '' on error.
    function HasParam(const Param: WideString): Boolean; overload;
    function HasParam(Param: Integer): Boolean; overload;
    function ParamCount: Integer;

    // moves to the next query (separated by ";" in SQL); bindings and result position are reset.
    function Next: Boolean;
    function HasAny: Boolean;
    procedure RunAll;

    // calls Next before returning; errors NIL is returned for non-selecting queries (e.g. UPDATE)
    // and empty result sets. If it returns non-NIL there's at least one row in the result.
    function Run: TSQLiteResult;
    procedure RunFreeing;   // the same as Run but frees the result, if any.
  end;

  TSQLiteResult = class (TSQLiteObject)
  protected
    FHandle: Pointer;
    FHasAny: Boolean;
    FDatabase: TSQLiteDatabase;

    FColumns: array[0..2047] of TSQLiteColumn;
    FSetCols: array[0..2047] of Boolean;

    function FetchColumn(Index: Integer): TSQLiteColumn;
    procedure ResetRow;
    procedure Close;
  public
    constructor Create(Handle: Pointer; DB: TSQLiteDatabase = NIL);
    destructor Destroy; override;

    function ColCount: Integer;
    function Column(Index: Integer): TSQLiteColumn; overload;
    function Column(const Name: WideString): TSQLiteColumn; overload;

    function RowCount: Integer;   // this will call Reset, iterate through all rows and Reset again.

    function Next: Boolean;
    // is set to False once Next returns False; Reset can be used to rewind the result set at any point.
    property HasAny: Boolean read FHasAny;
    procedure Reset;

    // Fields = [item[, item[, ...]]
    //   when item is String <col> or <col> followed by Integer Size:
    //     <col> = Integer (col index) or String/PChar (col name); Size = forced field size
    //     (optional for integer/double fields, defaults to 4; strings must not have this specified)
    //     You can retrieve 64-bit integers by setting Size to 5 or above.
    //   when item is Double (e.g. 3.0) - specifies the number of bytes to skip (do not fill)
    //     Note that this must be Double, not Integer, to avoid confusion with Size (above).
    // sqText items are WideString fields while sqBlob items are String.
    // Fields must end on a @Var item. Returns the number of bytes that were written to Rec.
    function ToRecord(var Rec; Fields: array of const): Integer;
    // before calling array at RecArray must have MaxCount length; returns -1 if there
    // were >= MaxCount rows, 0 if none or the number of stored rows.
    // TailSpace assumes each RecArray item has given number of extra bytes after the main
    //           fields (in record's beginning) that are filled from Fields.
    // WARNING: make sure to declare records as packed or calculate correct TailSpace otherwise.
    function ToRecords(var RecArray; MaxCount: Integer; Fields: array of const;
      TailSpace: Integer = 0): Integer;
  end;

    TSQLiteEmptyResult = class (TSQLiteResult)
    public
      constructor Create; reintroduce;
    end;

  TSQLiteBlob = class (TStream)
  protected
    FHandle: Pointer;
    FPosition: Integer;

    FDatabase: TSQLiteDatabase;
    FTable: TSQLiteTable;
    FColumn: WideString;
    FRowID: Int64;
    FIsReadOnly: Boolean;

    function GetSize: Int64; override;
    procedure SetSize(NewSize: LongInt); override;
  public
    constructor Acquire(Handle: Pointer);
    destructor Destroy; override;

    procedure Reopen(RowID: Int64);
    procedure Check(Operation: String; Status: Integer);

    property Database: TSQLiteDatabase read FDatabase;
    property Table: TSQLiteTable read FTable;
    property Column: WideString read FColumn;
    property RowID: Int64 read FRowID write Reopen;
    property IsReadOnly: Boolean read FIsReadOnly;

    function Read(var Buffer; Count: LongInt): Longint; override;
    function Write(const Buffer; Count: LongInt): Longint; override;
    function Seek(Offset: LongInt; Origin: Word): LongInt; override;
  end;

const
  SQLiteDLL = 'sqlite3.dll';

function sqlite3_aggregate_context(Context: Pointer; nBytes: Integer): Pointer; cdecl; external SQLiteDLL;
function sqlite3_aggregate_count(Context: Pointer): Integer; cdecl; external SQLiteDLL;
function sqlite3_bind_blob(pStmt: Pointer; ParamIdx: Integer; const Data; nData: Integer; xDel: TSQLiteBDestructor): Integer; cdecl; external SQLiteDLL;
function sqlite3_bind_double(pStmt: Pointer; ParamIdx: Integer; Data: Double): Integer; cdecl; external SQLiteDLL;
function sqlite3_bind_int(pStmt: Pointer; ParamIdx: Integer; Data: Integer): Integer; cdecl; external SQLiteDLL;
function sqlite3_bind_int64(pStmt: Pointer; ParamIdx: Integer; Data: Int64): Integer; cdecl; external SQLiteDLL;
function sqlite3_bind_null(pStmt: Pointer; ParamIdx: Integer): Integer; cdecl; external SQLiteDLL;
function sqlite3_bind_parameter_count(pStmt: Pointer): Integer; cdecl; external SQLiteDLL;
function sqlite3_bind_parameter_index(pStmt: Pointer; zName: PChar): Integer; cdecl; external SQLiteDLL;
function sqlite3_bind_parameter_name(pStmt: Pointer; n: Integer): PChar; cdecl; external SQLiteDLL;
function sqlite3_bind_text(pStmt: Pointer; ParamIdx: Integer; Data: PChar; nData: Integer; xDel: TSQLiteBDestructor): Integer; cdecl; external SQLiteDLL;
function sqlite3_bind_text16(pStmt: Pointer; ParamIdx: Integer; Data: PWideChar; nData: Integer; xDel: TSQLiteBDestructor): Integer; cdecl; external SQLiteDLL;
function sqlite3_bind_zeroblob(pStmt: Pointer; ParamIdx: Integer; nData: Integer): Integer; cdecl; external SQLiteDLL;
function sqlite3_blob_bytes(Blog: Pointer): Integer; cdecl; external SQLiteDLL;
function sqlite3_blob_close(Blog: Pointer): Integer; cdecl; external SQLiteDLL;
function sqlite3_blob_open(DB: Pointer; zDB, zTable, zColumn: PChar; RowID: Int64; Flags: Integer; out Blog: Pointer): Integer; cdecl; external SQLiteDLL;
function sqlite3_blob_read(Blog: Pointer; var Buf; Size, FromOffset: Integer): Integer; cdecl; external SQLiteDLL;
function sqlite3_blob_reopen(Blog: Pointer; RowID: Int64): Integer; cdecl; external SQLiteDLL;
function sqlite3_blob_write(Blog: Pointer; const Buf; Size, AtOffset: Integer): Integer; cdecl; external SQLiteDLL;
function sqlite3_busy_timeout(DB: Pointer; ms: Integer): Integer; cdecl; external SQLiteDLL;
function sqlite3_changes(DB: Pointer): Integer; cdecl; external SQLiteDLL;
function sqlite3_clear_bindings(pStmt: Pointer): Integer; cdecl; external SQLiteDLL;
function sqlite3_close(DB: Pointer): Integer; cdecl; external SQLiteDLL;
function sqlite3_column_blob(pStmt: Pointer; iCol: Integer): Pointer; cdecl; external SQLiteDLL;
function sqlite3_column_bytes(pStmt: Pointer; iCol: Integer): Integer; cdecl; external SQLiteDLL;
function sqlite3_column_bytes16(pStmt: Pointer; iCol: Integer): Integer; cdecl; external SQLiteDLL;
function sqlite3_column_count(pStmt: Pointer): Integer; cdecl; external SQLiteDLL;
function sqlite3_column_database_name(pStmt: Pointer; N: Integer): PChar; cdecl; external SQLiteDLL;
function sqlite3_column_database_name16(pStmt: Pointer; N: Integer): PWideChar; cdecl; external SQLiteDLL;
function sqlite3_column_decltype(pStmt: Pointer; i: Integer): PChar; cdecl; external SQLiteDLL;
function sqlite3_column_decltype16(pStmt: Pointer; i: Integer): PWideChar; cdecl; external SQLiteDLL;
function sqlite3_column_double(pStmt: Pointer; iCol: Integer): Double; cdecl; external SQLiteDLL;
function sqlite3_column_int(pStmt: Pointer; iCol: Integer): Integer; cdecl; external SQLiteDLL;
function sqlite3_column_int64(pStmt: Pointer; iCol: Integer): Int64; cdecl; external SQLiteDLL;
function sqlite3_column_name(pStmt: Pointer; n: Integer): PChar; cdecl; external SQLiteDLL;
function sqlite3_column_name16(pStmt: Pointer; n: Integer): PWideChar; cdecl; external SQLiteDLL;
function sqlite3_column_origin_name(pStmt: Pointer; N: Integer): PChar; cdecl; external SQLiteDLL;
function sqlite3_column_origin_name16(pStmt: Pointer; N: Integer): PWideChar; cdecl; external SQLiteDLL;
function sqlite3_column_table_name(pStmt: Pointer; N: Integer): PChar; cdecl; external SQLiteDLL;
function sqlite3_column_table_name16(pStmt: Pointer; N: Integer): PWideChar; cdecl; external SQLiteDLL;
function sqlite3_column_text(pStmt: Pointer; iCol: Integer): PChar; cdecl; external SQLiteDLL;
function sqlite3_column_text16(pStmt: Pointer; iCol: Integer): PWideChar; cdecl; external SQLiteDLL;
function sqlite3_column_type(pStmt: Pointer; iCol: Integer): Integer; cdecl; external SQLiteDLL;
function sqlite3_commit_hook(DB: Pointer; Callback: TSQLiteCommitCallback; Data: Pointer): TSQLiteCommitCallback; cdecl; external SQLiteDLL;
function sqlite3_compileoption_get(N: Integer): PChar; cdecl; external SQLiteDLL;
function sqlite3_compileoption_used(zOptName: PChar): Boolean; cdecl; external SQLiteDLL;
function sqlite3_complete(SQL: PChar): Integer; cdecl; external SQLiteDLL;
function sqlite3_complete16(SQL: PWideChar): Integer; cdecl; external SQLiteDLL;
function sqlite3_data_count(pStmt: Pointer): Integer; cdecl; external SQLiteDLL;
function sqlite3_db_handle(pStmt: Pointer): Pointer; cdecl; external SQLiteDLL;
function sqlite3_db_status(DB: Pointer; Op: Integer; pCurrent, pHighwater: PInteger; resetFlag: Boolean): Integer; cdecl; external SQLiteDLL;
function sqlite3_enable_load_extension(DB: Pointer; OnOff: Boolean): Integer; cdecl; external SQLiteDLL;
function sqlite3_enable_shared_cache(OnOff: Integer): Integer; cdecl; external SQLiteDLL;
function sqlite3_errcode(DB: Pointer): Integer; cdecl; external SQLiteDLL;
function sqlite3_errmsg(DB: Pointer): PChar; cdecl; external SQLiteDLL;
function sqlite3_errmsg16(DB: Pointer): PWideChar; cdecl; external SQLiteDLL;
function sqlite3_exec(DB: Pointer; SQLStatement: PChar; Callback: Pointer; UserData: Pointer; out ErrMsg: PChar): Integer; cdecl; external SQLiteDLL;
function sqlite3_expired(pStmt: Pointer): Integer; cdecl; external SQLiteDLL;
function sqlite3_extended_errcode(DB: Pointer): Integer; cdecl; external SQLiteDLL;
function sqlite3_extended_result_codes(DB: Pointer; OnOff: Boolean): Integer; cdecl; external SQLiteDLL;
function sqlite3_finalize(pStmt: Pointer): Integer; cdecl; external SQLiteDLL;
function sqlite3_get_autocommit(DB: Pointer): Boolean; cdecl; external SQLiteDLL;
function sqlite3_get_table(DB: Pointer; SQL: PChar; out resultp: PCharArray; out nrow: Integer; out column: Integer; out ErrMsg: PChar): Integer; cdecl; external SQLiteDLL;
function sqlite3_global_recover: Integer; cdecl; external SQLiteDLL;
function sqlite3_last_insert_rowid(DB: Pointer): Int64; cdecl; external SQLiteDLL;
function sqlite3_libversion: PChar; cdecl; external SQLiteDLL;
function sqlite3_libversion_number: Integer; cdecl; external SQLiteDLL;
function sqlite3_limit(DB: Pointer; Op: Integer; NewValue: Integer = -1): Integer; cdecl; external SQLiteDLL;
function sqlite3_load_extension(DB: Pointer; zFile: PChar; zProc: PChar = NIL; pzErrMsg: PPChar = NIL): Integer; cdecl; external SQLiteDLL;
function sqlite3_malloc(Size: Integer): Pointer; cdecl; external SQLiteDLL;
function sqlite3_memory_highwater(resetFlag: Boolean): Int64; cdecl; external SQLiteDLL;
function sqlite3_memory_used: Int64; cdecl; external SQLiteDLL;
function sqlite3_open(FileName: PChar; out ppDB: Pointer): Integer; cdecl; external SQLiteDLL;
function sqlite3_open_v2(FileName: PChar; out ppDB: Pointer; flags: Integer; zVfs: PChar): Integer; cdecl; external SQLiteDLL;
function sqlite3_open16(FileName: PWideChar; out ppDB: Pointer): Integer; cdecl; external SQLiteDLL;
function sqlite3_prepare(DB: Pointer; SQL: PChar; nBytes: Integer; out ppStmt: Pointer; out pzTail: PChar): Integer; cdecl; external SQLiteDLL;
function sqlite3_prepare_v2(DB: Pointer; SQL: PChar; nBytes: Integer; out ppStmt: Pointer; out pzTail: PChar): Integer; cdecl; external SQLiteDLL;
function sqlite3_prepare16(DB: Pointer; SQL: PWideChar; nBytes: Integer; out ppStmt: Pointer; out pzTail: PWideChar): Integer; cdecl; external SQLiteDLL;
function sqlite3_prepare16_v2(DB: Pointer; SQL: PWideChar; nBytes: Integer; out ppStmt: Pointer; out pzTail: PWideChar): Integer; cdecl; external SQLiteDLL;
function sqlite3_realloc(Buf: Pointer; Size: Integer): Pointer; cdecl; external SQLiteDLL;
function sqlite3_release_memory(Amount: Integer): Integer; cdecl; external SQLiteDLL;
function sqlite3_reset(pStmt: Pointer): Integer; cdecl; external SQLiteDLL;
function sqlite3_rollback_hook(DB: Pointer; Callback: TSQLiteRollbackCallback; Data: Pointer): TSQLiteRollbackCallback; cdecl; external SQLiteDLL;
function sqlite3_sleep(ms: Integer): Integer; cdecl; external SQLiteDLL;
function sqlite3_soft_heap_limit64(NewLimit: Int64): Int64; cdecl; external SQLiteDLL;
function sqlite3_sourceid: PChar; cdecl; external SQLiteDLL;
function sqlite3_sql(pStmt: Pointer): PChar; cdecl; external SQLiteDLL;
function sqlite3_status(Op: Integer; pCurrent, pHighwater: PInteger; resetFlag: Boolean): Integer; cdecl; external SQLiteDLL;
function sqlite3_step(pStmt: Pointer): Integer; cdecl; external SQLiteDLL;
function sqlite3_stmt_readonly(pStmt: Pointer): Boolean; cdecl; external SQLiteDLL;
function sqlite3_stmt_status(stmt: Pointer; op: Integer; resetFlg: Boolean): Integer; cdecl; external SQLiteDLL;
function sqlite3_strnicmp(First, Second: PChar; Length: Integer): Integer; cdecl; external SQLiteDLL;
function sqlite3_threadsafe: Boolean; cdecl; external SQLiteDLL;
function sqlite3_total_changes(DB: Pointer): Integer; cdecl; external SQLiteDLL;
function sqlite3_update_hook(DB: Pointer; Callback: TSQLiteUpdateCallback; Data: Pointer): TSQLiteRollbackCallback; cdecl; external SQLiteDLL;
function sqlite3_user_data(Context: Pointer): Pointer; cdecl; external SQLiteDLL;
function sqlite3_value_blob(Value: Pointer): Pointer; cdecl; external SQLiteDLL;
function sqlite3_value_bytes(Value: Pointer): Integer; cdecl; external SQLiteDLL;
function sqlite3_value_bytes16(Value: Pointer): Integer; cdecl; external SQLiteDLL;
function sqlite3_value_double(Value: Pointer): Double; cdecl; external SQLiteDLL;
function sqlite3_value_int(Value: Pointer): Integer; cdecl; external SQLiteDLL;
function sqlite3_value_int64(Value: Pointer): Int64; cdecl; external SQLiteDLL;
function sqlite3_value_numeric_type(Value: Pointer): Integer; cdecl; external SQLiteDLL;
function sqlite3_value_text(Value: Pointer): PChar; cdecl; external SQLiteDLL;
function sqlite3_value_text16(Value: Pointer): PWideChar; cdecl; external SQLiteDLL;
function sqlite3_value_text16be(Value: Pointer): PWideChar; cdecl; external SQLiteDLL;
function sqlite3_value_text16le(Value: Pointer): PWideChar; cdecl; external SQLiteDLL;
function sqlite3_value_type(Value: Pointer): Integer; cdecl; external SQLiteDLL;
function sqlite3_vmprintf(Format: PChar; const va_list: TVAList): PChar; cdecl; external SQLiteDLL;
function sqlite3_vsnprintf(Length: Integer; Output, Format: PChar; const va_list: TVAList): PChar; cdecl; external SQLiteDLL;
procedure sqlite3_busy_handler(DB: Pointer; Callback: TSQLiteBusyCallback; Data: Pointer); cdecl; external SQLiteDLL;
procedure sqlite3_free(p: Pointer); cdecl; external SQLiteDLL;
procedure sqlite3_free_table(var resultp: PCharArray); cdecl; external SQLiteDLL;
procedure sqlite3_interrupt(DB: Pointer); cdecl; external SQLiteDLL;
procedure sqlite3_progress_handler(DB: Pointer; OpCodeCount: Integer; Callback: TSQLiteProgressCallback; Data: Pointer); cdecl; external SQLiteDLL;
procedure sqlite3_randomness(N: Integer; P: Pointer); cdecl; external SQLiteDLL;
procedure sqlite3_reset_auto_extension; cdecl; external SQLiteDLL;

function SQLiteErrorMsg(Err: Integer; Msg: PPChar): string;
procedure SQLiteExec(DB: PPointer; const S: string);
{ IfEmpty: SQLite would behave differently depending on NIL or '' PChar - for example,
           sqlite3_open_v2(, , , VFS='') would always fail while sqlite3_open_v2(, , , VFS=NIL) won't. }
{ WARNING: do not use 2 overloaded functions under the same name (e.g. SQLiteStr both for
           wide and ANSI inputs) - Delphi will often confuse which one to use and this
           will lead to some very tricky bugs!                                            }
function ToSQLiteStr(const Wide: WideString; IfEmpty: PChar = NIL): PChar;
function FromSQLiteStr(UTF8: PChar): WideString;
function SQLiteType(Kind: Integer): TSQLiteType;
procedure SQLiteRaise(DB: Pointer; E: ESQLite); overload;
procedure SQLiteRaise(DB: TSQLiteDatabase; E: ESQLite); overload;


const
  SQLITE_STATIC                          = Pointer(0);
  SQLITE_TRANSIENT                       = Pointer(-1);

implementation

type
  // used in TSQLiteResult.ToRecord/s:
  TRec = array[0..$1000000 {16 MiB}] of Byte;

function SQLiteErrorMsg(Err: Integer; Msg: PPChar): string;
begin
  Result := TSQLite.Error(Err);

  if Msg <> NIL then
  begin
    Result := Result + ': ' + StrPas(@Msg^);
    sqlite3_free(Msg);
  end;
end;

procedure SQLiteExec(DB: PPointer; const S: string);
var
  Error: PChar;
  Res: Integer;
begin
  Res := sqlite3_exec(Db, PChar(S), NIL, NIL, Error);
  if Res <> SQLITE_OK then
    SQLiteRaise(DB, ESQLite.Create(Res, Error));
end;

function ToSQLiteStr(const Wide: WideString; IfEmpty: PChar = NIL): PChar;
begin
  if Length(Wide) = 0 then
    Result := IfEmpty
    else
      Result := PChar(UTF8Encode(Wide));
end;

function FromSQLiteStr(UTF8: PChar): WideString;
begin
  if Length(UTF8) = 0 then
    Result := ''
    else
      Result := UTF8Decode(UTF8);
end;

function SQLiteType(Kind: Integer): TSQLiteType;
begin
  Dec(Kind);  // SQLITE_INTEGER = 1

  if (Kind < Integer(Low(Result))) or (Kind > Integer(High(Result))) then
    raise ESQLiteInvalidType.Create(Kind)
    else
      Result := TSQLiteType(Kind);
end;

procedure SQLiteRaise(DB: Pointer; E: ESQLite); overload;
begin
  E.FillWithInfoFrom(DB);
  raise E;
end;

procedure SQLiteRaise(DB: TSQLiteDatabase; E: ESQLite); overload;
begin
  if DB = NIL then
    raise E
    else
    begin
      E.DbObject := DB;
      SQLiteRaise(DB.Handle, E);
    end;
end;

{ ESQLite }

constructor ESQLite.Create(Code: Integer; Msg: String);
begin
  FMessageUpdated := False;

  Self.Code := Code;
  Self.Msg := Msg;

  if Msg = '' then
    Msg := TSQLite.Error(Code)
    else if Code >= 0 then
      Msg := TSQLite.Error(Code) + ': ' + Msg;
  inherited Create(Msg);
end;

constructor ESQLite.Create(Code: Integer; Msg: PPChar);
begin
  if Msg = NIL then
    Create(Code)
    else
    begin
      Create(Code, StrPas(Msg^));
      sqlite3_free(Msg);
    end;
end;

procedure ESQLite.FillWithInfoFrom(DB: Pointer);
begin
  if DB <> NIL then
  begin
    LastError := sqlite3_errcode(DB);
    LastErrorExt := sqlite3_extended_errcode(DB);
    LastErrorMsg := FromSQLiteStr( sqlite3_errmsg(DB) );

    if not FMessageUpdated then
    begin
      FMessageUpdated := True;

      if LastError <> 0 then
        Message := Message + #10'SQLite code: ' + IntToStr(LastError);
      if (LastErrorExt <> 0) and (LastErrorExt <> LastError) then
        Message := Message + #10' / ' + IntToStr(LastErrorExt) + ' extended';
      if LastErrorMsg <> '' then
        Message := Message + #10'Message: ' + LastErrorMsg;
    end;
  end;
end;

{ Exceptions }

constructor ESQLiteOpenDB.Create(Code: Integer; const FN: WideString; Flags: Integer; const VFS: WideString);
begin
  inherited Create(Code, FN);

  Self.FN := FN;
  Self.Flags := Flags;
  Self.VFS := VFS;
end;

constructor ESQLiteLoadingExtension.Create(Code: Integer; Msg: PPChar; const FN, EntryPoint: WideString);
begin
  inherited Create(Code, Msg);

  Self.FN := FN;
  Self.EntryPoint := EntryPoint;
end;

constructor ESQLiteInvalidType.Create(Kind: Integer);
begin
  inherited Create(-1, 'SQLite type constant (' + IntToStr(Kind) + ') cannot be converted into TSQLiteType.');
  Self.Kind := Kind;
end;

constructor ESQLiteUnsupportedPrintfArg.Create(VType: Integer);
begin
  inherited Create(-1, 'TSQLite.Printf was passed an unsupported argument with VType = ' + IntToStr(VType));
  Self.VType := VType;
end;

constructor ESQLiteInvalidUpdateAction.Create(Action: Integer);
begin
  inherited Create(-1, 'SQLite action constant (' + IntToStr(Action) + ') cannot be converted into TSQLiteAction.');
  Self.Action := Action;
end;

constructor ESQLitePreparingSQL.Create(Code: Integer; const SQL: WideString);
begin
  inherited Create(Code, SQL);
  Self.SQL := SQL;
end;

constructor ESQLiteEmptySQL.Create;
begin
  inherited Create(-1, 'Empty SQL statement.');
end;

constructor ESQLiteNoColumnByIndex.Create(Index: Integer);
begin
  inherited Create(-1, 'No column with index ' + IntToStr(Index) + ' in the result set.');
  Self.Index := Index;
end;

constructor ESQLiteNoColumnByName.Create(const Name, All: WideString);
begin
  inherited Create(-1, 'No column named "' + Name + '" in the result set; have columns: ' + All + '.');

  Self.Name := Name;
  Self.All := All;
end;

constructor ESQLiteNoMoreResults.Create(const Operation: WideString);
begin
  Self.Operation := Operation;
  inherited Create(-1, 'Attempted to access (' + Operation + ') of the result set that had no more rows left.');
end;

constructor ESQLiteBind.Create(Code, Param: Integer);
begin
  inherited Create(Code, 'Error binding parameter #' + IntToStr(Param));
  ParamIndex := Param;
  ParamName := '';
end;

constructor ESQLiteResultToRecord.Create(Reason: WideString);
begin
  Code := -1;
  Msg := Reason;

  CreateFmt('Error calling TSQLiteResult.ToRecord* - %s.', [Reason]);
end;

constructor ESQLiteBind.Create(Code: Integer; const Param: WideString);
begin
  inherited Create(Code, 'Error binding parameter "' + Param + '"');
  ParamIndex := 0;
  ParamName := Param;
end;

constructor ESQLiteEmptyBindParam.Create(const Param: WideString);
begin
  ESQLite(Self).Create( -1, WideFormat('TSQLiteQuery.Bind was passed empty param name ("%s")', [Param]) );
end;

constructor ESQLiteUnsupportedBindArg.Create(Param: WideString; VType: Integer);
begin
  ESQLite(Self).Create( -1, WideFormat('TSQLiteQuery.Bind was passed an unsupported VType %d of param %s', [VType, Param]) );
  Self.ParamName := Param;
  Self.VType := VType;
end;

constructor ESQLiteUnsupportedBindArg.Create(Param, VType: Integer);
begin
  ESQLite(Self).Create( -1, WideFormat('TSQLiteQuery.Bind was passed an unsupported VType %d of param #%d', [VType, Param]) );
  Self.ParamIndex := Param;
  Self.VType := VType;
end;

constructor ESQLiteOpenTableBlob.Create(Code: Integer; const DB, Table, Field: WideString; RowID: Int64);
begin
  Self.DB := DB;
  Self.Table := Table;
  Self.Field := Field;
  Self.RowID := RowID;

  Msg := Format('Error opening BLOB field %s of row %d of table %s.%s.', [Field, RowID, DB, Table]);
  inherited Create(Code, Msg);
end;

constructor ESQLiteSettingBlobSize.Create(OldSize, NewSize: Integer);
begin
  Msg := Format('SQLite does not support changing BLOB sizes (old size = %d, new size = %d).', [OldSize, NewSize]);
  inherited Create(-1, Msg);

  Self.OldSize := OldSize;
  Self.NewSize := NewSize;
end;

constructor ESQLiteBlobOperation.Create(Code: Integer; Operation: String);
begin
  Self.Operation := Operation;
  inherited Create(Code, 'Error ' + Operation + 'ing BLOB field');
end;

{ TSQLite }

class function TSQLite.Version: WideString;
begin
  Result := FromSQLiteStr(sqlite3_libversion);
end;

class function TSQLite.VersionNum: Integer;
begin
  Result := sqlite3_libversion_number;
end;

class function TSQLite.SourceID: WideString;
begin
  Result := FromSQLiteStr(sqlite3_sourceid);
end;

class function TSQLite.Quote(const Str: WideString): WideString;
begin
  Result := TSQLite.Printf('%q', [Str]);
end;

class function TSQLite.QuoteWrapping(const Str: WideString): WideString;
begin
  Result := TSQLite.Printf('%Q', [Str]);
end;

class function TSQLite.QuoteOrNull(const Str: WideString): WideString;
begin
  if Str = '' then
    Result := 'NULL'
    else
      Result := QuoteWrapping(Str);
end;

class function TSQLite.IsThreadSafe: Boolean;
begin
  Result := sqlite3_threadsafe;
end;

class function TSQLite.HasCompileOption(const Name: WideString): Boolean;
begin
  Result := sqlite3_compileoption_used(ToSQLiteStr(Name));
end;

class function TSQLite.CompileOptions(WithPrefix: Boolean = False): TStrings;
var
  I: Integer;
  Option: String;
begin
  Result := TStringList.Create;
  try
    for I := 0 to MaxInt do
    begin
      Option := sqlite3_compileoption_get(I);
      if Option = '' then
        Break;

      if WithPrefix then
        Option := 'SQLITE_' + Option;
      Result.Add(Option);
    end;
  except
    Result.Free;
    raise;
  end;
end;

class function TSQLite.Random(Size: Integer): TMemoryStream;
var
  Buf: Pointer;
begin
  Result := TMemoryStream.Create;
  try
    Buf := sqlite3_malloc(5);
    try
      sqlite3_randomness(Size, Buf);
      Result.WriteBuffer(Buf^, Size);
      Result.Position := 0;
    finally
      sqlite3_free(Buf);
    end;
  except
    Result.Free;
    raise;
  end;
end;

class function TSQLite.ReleaseMemory(Amount: Integer): Integer;
begin
  Result := sqlite3_release_memory(Amount);
end;

class function TSQLite.SoftHeapLimit(NewLimit: Int64): Int64;
begin
  Result := sqlite3_soft_heap_limit64(NewLimit);
end;

class function TSQLite.Error(Err: Integer): WideString;
begin
  case Err of
  SQLITE_OK           : Result := 'Successful result';
  SQLITE_ERROR        : Result := 'SQL error or missing database';
  SQLITE_INTERNAL     : Result := 'An internal logic error in SQLite';
  SQLITE_PERM         : Result := 'Access permission denied';
  SQLITE_ABORT        : Result := 'Callback routine requested an abort';
  SQLITE_BUSY         : Result := 'The database file is locked';
  SQLITE_LOCKED       : Result := 'A table in the database is locked';
  SQLITE_NOMEM        : Result := 'A malloc() failed';
  SQLITE_READONLY     : Result := 'Attempt to write a readonly database';
  SQLITE_INTERRUPT    : Result := 'Operation terminated by sqlite_interrupt()';
  SQLITE_IOERR        : Result := 'Disk I/O error occurred';
  SQLITE_CORRUPT      : Result := 'The database disk image is malformed';
  SQLITE_NOTFOUND     : Result := '(Internal Only) Table or record not found';
  SQLITE_FULL         : Result := 'Insertion failed because database is full';
  SQLITE_CANTOPEN     : Result := 'Unable to open the database file';
  SQLITE_PROTOCOL     : Result := 'Database lock protocol error';
  SQLITE_EMPTY        : Result := '(Internal Only) Database table is empty';
  SQLITE_SCHEMA       : Result := 'The database schema changed';
  SQLITE_TOOBIG       : Result := 'Too much data for one row of a table';
  SQLITE_CONSTRAINT   : Result := 'Abort due to contraint violation';
  SQLITE_MISMATCH     : Result := 'Data type mismatch';
  SQLITE_MISUSE       : Result := 'Library used incorrectly';
  SQLITE_NOLFS        : Result := 'Used OS features not supported on host';
  SQLITE_AUTH         : Result := 'Authorization denied';
  SQLITE_ROW          : Result := 'sqlite_step() has another row ready';
  SQLITE_DONE         : Result := 'sqlite_step() has finished executing';
  else
    Result := Format('Unknown SQLite error (%d)', [Err]);
  end;
end;

class function TSQLite.Printf(Format: WideString; const Args: array of const): WideString;
var
  VA: TVAList;
  StrI, I: Integer;
  Strings: array[0..High(VA)] of String;
  Res: PChar;
begin
  StrI := 0;

  for I := 0 to Length(Args) - 1 do
    with Args[I] do
      if I >= Length(VA) then
        Exit
        else
          // SQLite supports %f but I couldn't get Delphi's VExtended work with it;
          // others (Integer, WideString, String) have been tested fine.
          case VType of
          vtInteger:      VA[I] := Pointer(VInteger);
          vtPChar,
          vtAnsiString:   VA[I] := VPChar;
          vtPWideChar,
          vtWideString:
            begin
              Strings[StrI] := ToSQLiteStr(VPWideChar, '') + #0;
              VA[I] := @Strings[StrI][1];
              Inc(StrI);
            end;
          else
            if (VType = vtPointer) and (VPointer = NIL) then
              VA[I] := NIL
              else
                raise ESQLiteUnsupportedPrintfArg.Create(VType);
          end;

  Res := sqlite3_vmprintf(ToSQLiteStr(Format, ''), VA);
  Result := FromSQLiteStr(Res);
  sqlite3_free(Res);
end;

class function TSQLite.Status(Op: Integer; out Current, Peak: Integer; Reset: Boolean = False): Integer;
var
  Cur, High: PInteger;
begin
  Current := 0;
  Peak := 0;

  Result := sqlite3_status(Op, @Cur, @High, Reset);
  if Result = SQLITE_OK then
  begin
    if Cur <> NIL then
      Current := Cur^;
    if High <> NIL then
      Peak := High^;
  end;
end;

class function TSQLite.MemoryUsed: Int64;
begin
  Result := sqlite3_memory_used;
end;

class function TSQLite.MemoryPeak(Reset: Boolean = False): Int64;
begin
  Result := sqlite3_memory_highwater(Reset);
end;

class function TSQLite.StrIComp(const S1, S2: WideString): Integer;
var
  S1Enc, S2Enc: PChar;
begin
  S1Enc := ToSQLiteStr(S1);
  S2Enc := ToSQLiteStr(S2);

  Result := Length(S1Enc);
  if Result > Length(S2Enc) then
    Result := Length(S2Enc);

  Result := sqlite3_strnicmp(S1Enc, S2Enc, Result);
end;

{ TSQLiteDatabase }

class function TSQLiteDatabase.Quote(const Str: WideString): WideString;
begin
  Result := TSQLite.Quote(Str);
end;

class function TSQLiteDatabase.QuoteWrapping(const Str: WideString): WideString;
begin
  Result := TSQLite.QuoteWrapping(Str);
end;

class function TSQLiteDatabase.QuoteOrNull(const Str: WideString): WideString;
begin
  Result := TSQLite.QuoteOrNull(Str);
end;

class function TSQLiteDatabase.GetTempFileName: WideString;
var
  Tries: Integer;
begin
  Result := Utils.GetTempFileName;

  if Result = '' then
  begin
    Tries := 0;

    repeat
      Result := IncludeTrailingPathDelimiter( ExtractFilePath(ParamStrW(0)) ) +
                'sqlite-' + IntToStr(Random(MaxInt)) + '.tmp';

      if Tries > 100 then
        Result := '';
    until not FileExists(Result) and (Result <> '');
  end;

  if Result = '' then
    raise Exception.Create('Cannot generate temporary file name for TSQLiteDatabase.');
end;

procedure TSQLiteDatabase.Init;
begin
  FOpenFileName := '';
  FOpenFlags := 0;
  FOpenVFS := '';

  FDeleteOnClose := False;
  FTransactionNesting := 0;
  FHandle := NIL;

  FOnProgress := NIL;
  FOnProgressOpCodeCount := 30;
  FOnBusy := NIL;
  FBusyTimeout := 0;
  FOnCommit := NIL;
  FOnRollback := NIL;
  FOnUpdate := NIL;

  FTables := TObjectHash.Create(True);
  FQueries := TSQLiteObjectList.Create;
end;

procedure TSQLiteDatabase.PrepareConnection;
begin
  sqlite3_extended_result_codes(FHandle, True);
end;

constructor TSQLiteDatabase.Acquire(Handle: Pointer);
begin
  Init;

  FHandle := Handle;
  PrepareConnection;
end;

constructor TSQLiteDatabase.CreateCustom(const FN: WideString; Flags: Integer; const VFS: WideString);
var
  Error: Integer;
begin
  Init;

  FOpenFileName := FN;
  FOpenFlags := Flags;
  FOpenVFS := VFS;

  Error := sqlite3_open_v2(ToSQLiteStr(FN), FHandle, Flags, ToSQLiteStr(VFS));
  if Error <> SQLITE_OK then
  begin
    Close;
    SQLiteRaise(Self, ESQLiteOpenDB.Create(Error, FN, Flags, VFS));
  end;

  PrepareConnection;
end;

constructor TSQLiteDatabase.CreateCustom(Stream: TStream; Flags: Integer);
var
  Temp: WideString;
  S: TFIleStreamW;
begin
  Temp := GetTempFileName;

  S := TFileStreamW.Create(Temp, fmCreate);
  try
    S.CopyFrom(Stream, Stream.Size - Stream.Position);
  finally
    Stream.Position := Stream.Size - S.Size;
    S.Free;
  end;

  try
    CreateCustom(Temp, Flags);
    FDeleteOnClose := True;
  except
    DeleteFileW(PWideChar(Temp));
    raise;
  end;
end;

  constructor TSQLiteDatabase.Create(const FN: WideString);
  begin
    CreateCustom(FN, SQLITE_OPEN_READWRITE or SQLITE_OPEN_CREATE);
  end;

  constructor TSQLiteDatabase.New(const FN: WideString);
  begin
    CreateCustom(FN, SQLITE_OPEN_CREATE);
  end;

  constructor TSQLiteDatabase.CreateInMemory;
  begin
    CreateCustom(':memory:', SQLITE_OPEN_CREATE);
  end;

  constructor TSQLiteDatabase.CreateTemp;
  begin
    CreateCustom('', SQLITE_OPEN_CREATE);
  end;

  constructor TSQLiteDatabase.Open(const FN: WideString);
  begin
    CreateCustom(FN, SQLITE_OPEN_READWRITE);
  end;

  constructor TSQLiteDatabase.Open(Stream: TStream);
  begin
    CreateCustom(Stream, SQLITE_OPEN_READWRITE);
  end;

  constructor TSQLiteDatabase.OpenReadOnly(const FN: WideString);
  begin
    CreateCustom(FN, SQLITE_OPEN_READONLY);
  end;

  constructor TSQLiteDatabase.OpenReadOnly(Stream: TStream);
  begin
    CreateCustom(Stream, SQLITE_OPEN_READONLY);
  end;

destructor TSQLiteDatabase.Destroy;
begin
  Close;

  FQueries.Free;
  FTables.Free;
  inherited;
end;

procedure TSQLiteDatabase.Close;
begin
  if FHandle <> NIL then
  begin
    FQueries.Clear;
    FTables.Clear;

    sqlite3_close(FHandle);
    FHandle := NIL;

    if FDeleteOnClose and (FOpenFileName <> '') then
    begin
      DeleteFileW(PWideChar(FOpenFileName));
      FDeleteOnClose := False;
    end;
  end;
end;

function TSQLiteDatabase.GetTable(Name: WideString): TSQLiteTable;
begin
  Result := TSQLiteTable(FTables[Name]);

  if Result = NIL then
  begin
    Result := TSQLiteTable.Create(Self, Name);
    FTables.AddObject(Name, Result);
  end;
end;

function TSQLiteDatabase.TableNames: TStringsW;
var
  Tables: TStringListW;
begin
  Tables := TStringListW.Create;
  try
    Result := Tables.NameList;
  finally
    Tables.Free;
  end;
end;

procedure TSQLiteDatabase.TablesOf(DB: WideString; Dest: TStringsW);
begin
  Dest.BeginUpdate;
  try
    DB := TSQLite.QuoteWrapping(DB);
    with QueryResult('SELECT name FROM ' + DB + '.sqlite_master WHERE type = ? ORDER BY name', ['table']) do
      try
        if HasAny then
          repeat
            Dest.Add(Column(0).Text);
          until not Next;
      finally
        Free;
      end;
  finally
    Dest.EndUpdate;
  end;
end;

function TSQLiteDatabase.NewQuery(const SQL: WideString; Bind: array of const): TSQLiteQuery;
begin
  Result := TSQLiteQuery.Create(SQL, Self);
  Result.Bind(Bind);
end;

function TSQLiteDatabase.QueryResult(const SQL: WideString; Bind: array of const;
  NilIfNone: Boolean = False): TSQLiteResult;
begin
  with NewQuery(SQL, Bind) do
    try
      Result := Run;
      if (Result = NIL) and not NilIfNone then
        Result := TSQLiteEmptyResult.Create;
    finally
      Free;
    end;
end;

function TSQLiteDatabase.Execute(const SQL: WideString; Bind: array of const): Integer;
begin
  with NewQuery(SQL, Bind) do
    try
      Result := 0;
      repeat
        RunFreeing;
        Inc(Result, LastChanged);
      until not HasAny;
    finally
      Free;
    end;
end;

function OnProgressCb(Data: Pointer): Boolean; cdecl;
begin
  if Assigned(TSQLiteDatabase(Data).FOnProgress) then
    Result := TSQLiteDatabase(Data).FOnProgress(TSQLiteDatabase(Data))
    else
      Result := False;
end;

  function OnBusyCb(Data: Pointer; TryCount: Integer): Boolean; cdecl;
  begin
    if Assigned(TSQLiteDatabase(Data).FOnBusy) then
      Result := TSQLiteDatabase(Data).FOnBusy(TSQLiteDatabase(Data), TryCount)
      else
        Result := False;
  end;

  function OnCommitCb(Data: Pointer): Boolean; cdecl;
  begin
    if Assigned(TSQLiteDatabase(Data).FOnCommit) then
      Result := TSQLiteDatabase(Data).FOnCommit(TSQLiteDatabase(Data))
      else
        Result := False;
  end;

  procedure OnRollbackCb(Data: Pointer) cdecl;
  begin
    if Assigned(TSQLiteDatabase(Data).FOnRollback) then
      TSQLiteDatabase(Data).FOnRollback(TSQLiteDatabase(Data));
  end;

  procedure OnUpdateCb(Data: Pointer; Action: Integer; Database, Table: PChar; RowID: Int64) cdecl;
  var
    Act: TSQLiteAction;
  begin
    if Assigned(TSQLiteDatabase(Data).FOnUpdate) then
    begin
      Act := sqInsert;    // compiler warning.
      case Action of
      SQLITE_INSERT: Act := sqInsert;
      SQLITE_DELETE: Act := sqDelete;
      SQLITE_UPDATE: Act := sqUpdate;
      else
        SQLiteRaise(TSQLiteDatabase(Data), ESQLiteInvalidUpdateAction.Create(Action));
      end;

      TSQLiteDatabase(Data).FOnUpdate(TSQLiteDatabase(Data), Act, TSQLiteDatabase(Data)[Table], RowID);
    end;
end;

procedure TSQLiteDatabase.SetOnProgress(Value: TSQLiteProgressEvent);
begin
  FOnProgress := Value;

  if Assigned(Value) then
    sqlite3_progress_handler(FHandle, FOnProgressOpCodeCount, OnProgressCb, Self)
    else
      sqlite3_progress_handler(FHandle, FOnProgressOpCodeCount, NIL, Self);
end;

  procedure TSQLiteDatabase.SetOnBusy(const Value: TSQLiteBusyEvent);
  begin
    FOnBusy := Value;

    if Assigned(Value) then
      sqlite3_busy_handler(FHandle, OnBusyCb, Self)
      else
        sqlite3_busy_handler(FHandle, NIL, Self);
  end;

  procedure TSQLiteDatabase.SetOnCommit(Value: TSQLiteCommitEvent);
  begin
    FOnCommit := Value;

    if Assigned(Value) then
      sqlite3_commit_hook(FHandle, OnCommitCb, Self)
      else
        sqlite3_commit_hook(FHandle, NIL, Self);
  end;

  procedure TSQLiteDatabase.SetOnRollback(Value: TSQLiteRollbackEvent);
  begin
    FOnRollback := Value;

    if Assigned(Value) then
      sqlite3_rollback_hook(FHandle, OnRollbackCb, Self)
      else
        sqlite3_rollback_hook(FHandle, NIL, Self);
  end;

  procedure TSQLiteDatabase.SetOnUpdate(Value: TSQLiteUpdateEvent);
  begin
    FOnUpdate := Value;

    if Assigned(Value) then
      sqlite3_update_hook(FHandle, OnUpdateCb, Self)
      else
        sqlite3_update_hook(FHandle, NIL, Self);
  end;

procedure TSQLiteDatabase.SetOnProgressOpCodeCount(const Value: Integer);
begin
  FOnProgressOpCodeCount := Value;
end;

procedure TSQLiteDatabase.SetBusyTimeout(Value: Integer);
begin
  FBusyTimeout := Value;
  sqlite3_busy_timeout(FHandle, Value);

  if Value <= 0 then
    SetOnBusy(FOnBusy);   // reassign previous custom OnBusy handler, if any.
end;

function TSQLiteDatabase.Status(Op: Integer; out Current, Peak: Integer; Reset: Boolean): Integer;
var
  Cur, High: PInteger;
begin
  Current := 0;
  Peak := 0;

  Result := sqlite3_db_status(FHandle, Op, @Cur, @High, Reset);
  if Result = SQLITE_OK then
  begin
    if Cur <> NIL then
      Current := Cur^;
    if High <> NIL then
      Peak := High^;
  end;
end;

procedure TSQLiteDatabase.AllowLoadingExtensions(Allow: Boolean);
var
  Code: Integer;
begin
  Code := sqlite3_enable_load_extension(FHandle, Allow);
  if Code <> SQLITE_OK then
    SQLiteRaise(Self, ESQLiteAllowingLoadExt.Create(Code));
end;

procedure TSQLiteDatabase.LoadExtension(const FN, EntryPoint: WideString);
var
  Code: Integer;
  Msg: PChar;
begin
  Code := sqlite3_load_extension(FHandle, ToSQLiteStr(FN), ToSQLiteStr(EntryPoint), @Msg);
  if Code <> SQLITE_OK then
    SQLiteRaise(Self, ESQLiteLoadingExtension.Create(Code, @Msg, FN, EntryPoint));
end;

function TSQLiteDatabase.GetLimit(Op: Integer): Integer;
begin
  Result := sqlite3_limit(FHandle, Op, -1);
end;

procedure TSQLiteDatabase.SetLimit(Op: Integer; Value: Integer);
begin
  sqlite3_limit(FHandle, Op, Value);
end;

function TSQLiteDatabase.LastInsertID: Int64;
begin
  Result := sqlite3_last_insert_rowid(FHandle);
end;

function TSQLiteDatabase.LastChanged: Integer;
begin
  Result := sqlite3_changes(FHandle);
end;

procedure TSQLiteDatabase.Interrupt;
begin
  sqlite3_interrupt(FHandle);
end;

function TSQLiteDatabase.IsAutocommitting: Boolean;
begin
  Result := sqlite3_get_autocommit(FHandle);
end;

function TSQLiteDatabase.LastError: Integer;
begin
  Result := sqlite3_errcode(FHandle);
end;

function TSQLiteDatabase.LastErrorExt: Integer;
begin
  Result := sqlite3_extended_errcode(FHandle);
end;

function TSQLiteDatabase.LastErrorMsg: WideString;
begin
  Result := FromSQLiteStr( sqlite3_errmsg(FHandle) );
end;

function TSQLiteDatabase.GetForeignKeys: Boolean;
begin
  with QueryResult('PRAGMA foreign_keys', []) do
    try
      Result := HasAny and Boolean(Column(0).Int);
    finally
      Free;
    end;
end;

procedure TSQLiteDatabase.SetForeignKeys(const Value: Boolean);
const
  States: array[0..1] of String = ('OFF', 'ON');
begin
  Execute('PRAGMA foreign_keys = ' + States[Byte(Value)], []);
end;

procedure TSQLiteDatabase.BeginTransaction(Kind: TSQLiteTransactionType);
const
  Kinds: array[TSQLiteTransactionType] of String = ('', ' DEFERRED', ' IMMEDIATE', ' EXCLUSIVE');
begin
  FTransactionNesting := 1;
  Execute('BEGIN' + Kinds[Kind] + ' TRANSACTION', []);
end;

procedure TSQLiteDatabase.Commit;
begin
  FTransactionNesting := 0;
  Execute('COMMIT', []);
end;

procedure TSQLiteDatabase.Rollback;
begin
  FTransactionNesting := 0;
  Execute('ROLLBACK', []);
end;

function TSQLiteDatabase.InTransaction: Boolean;
begin
  Result := FTransactionNesting > 0;
end;

function TSQLiteDatabase.BeginNestedTransaction(Kind: TSQLiteTransactionType): Boolean;
begin
  Result := not InTransaction;
  if Result then
    BeginTransaction(Kind)
    else
      Inc(FTransactionNesting);
end;

function TSQLiteDatabase.CommitNested: Boolean;
begin
  Result := FTransactionNesting = 1;
  if Result then
    Commit
    else
      Dec(FTransactionNesting);
end;

{ TSQLiteTable }

constructor TSQLiteTable.Create(DB: TSQLiteDatabase; const Name: WideString);
begin
  FDatabase := DB;
  FName := Name;

  FBlobs := TSQLiteObjectList.Create;
end;

destructor TSQLiteTable.Destroy;
begin
  FBlobs.Free;
  inherited;
end;

function TSQLiteTable.OpenBlob(Field: WideString; RowID: Int64; ReadOnly: Boolean = False;
  DB: WideString = 'main'): TSQLiteBlob;
var
  Error: Integer;
  Handle: Pointer;
begin
  Error := sqlite3_blob_open(FDatabase.Handle, ToSQLiteStr(DB), ToSQLiteStr(FName),
                             ToSQLiteStr(Field), RowID, Integer(not ReadOnly), Handle);

  if Error = SQLITE_OK then
    Result := TSQLiteBlob.Acquire(Handle)
    else
    begin
      sqlite3_blob_close(Handle);
      SQLiteRaise(FDatabase, ESQLiteOpenTableBlob.Create(Error, DB, FName, Field, RowID));
      Result := NIL;
    end;
end;

function TSQLiteTable.QueryCount(Where: WideString): Int64;
begin
  Result := QueryCount(Where, []);
end;

function TSQLiteTable.QueryCount(Where: WideString; Bind: array of const): Int64;
begin
  if Where <> '' then
    Where := ' WHERE ' + Where;
  Where := '"' + FName + '"';

  with FDatabase.QueryResult('SELECT COUNT(1) FROM ' + Where, Bind) do
    try
      Result := Column(0).Int64;
    finally
      Free;
    end;
end;

{ TSQLiteQuery }

constructor TSQLiteQuery.Create(const SQL: WideString; DB: TSQLiteDatabase);
begin
  FFullSQL := SQL;
  FNextSQL := SQL;

  FDatabase := DB;
  DB.FQueries.Add(Self);

  FHandle := PrepareNext;
end;

destructor TSQLiteQuery.Destroy;
begin
  sqlite3_finalize(FHandle);
  FDatabase.FQueries.Extract(Self);
  inherited;
end;

function TSQLiteQuery.PrepareNext: Pointer;
var
  Tail: PChar;
  Error: Integer;
begin
  Result := NIL;

  if FNextSQL <> '' then
  begin
    Error := sqlite3_prepare_v2(FDatabase.FHandle, ToSQLiteStr(FNextSQL), -1, Result, Tail);

    if (Error = SQLITE_OK) and (Result <> NIL) then
      FNextSQL := Tail
      else
      begin
        sqlite3_finalize(Result);
        SQLiteRaise(FDatabase, ESQLitePreparingSQL.Create(Error, FNextSQL));
      end;
  end;
end;

procedure TSQLiteQuery.Reset;
begin
  if FHandle <> NIL then
    sqlite3_reset(FHandle);
end;

procedure TSQLiteQuery.ClearBindings;
begin
  if FHandle <> NIL then
    sqlite3_clear_bindings(FHandle);
end;

function TSQLiteQuery.Next: Boolean;
begin
  sqlite3_finalize(FHandle);
  FHandle := NIL;

  FHandle := PrepareNext;
  Result := HasAny;
end;

function TSQLiteQuery.HasAny: Boolean;
begin
  Result := FHandle <> NIL;
end;

function TSQLiteQuery.Execute(FailOnSqlError: Boolean = True): Integer;
begin
  if not HasAny then
    SQLiteRaise(FDatabase, ESQLiteEmptySQL.Create);

  Result := sqlite3_step(FHandle);
  if FailOnSqlError and (Result <> SQLITE_OK) and (Result <> SQLITE_DONE) and (Result <> SQLITE_ROW) then
    SQLiteRaise(FDatabase, ESQLiteExecutingSQL.Create(Result, FFullSQL));
end;

procedure TSQLiteQuery.RunAll;
begin
  while Next do
    Execute;
end;

function TSQLiteQuery.Run: TSQLiteResult;
begin
  if Execute = SQLITE_ROW then
  begin
    Result := TSQLiteResult.Create(FHandle);
    FHandle := NIL;
  end
    else
      Result := NIL;

  try
    Next;
  except
    Result.Free;
    raise;
  end;
end;

function TSQLiteQuery.IsReadOnly: Boolean;
begin
  Result := sqlite3_stmt_readonly(FHandle);
end;

function TSQLiteQuery.Status(Op: Integer; Reset: Boolean): Integer;
begin
  Result := sqlite3_stmt_status(FHandle, Op, Reset);
end;

procedure TSQLiteQuery.RunFreeing;
var
  Res: TSQLiteResult;
begin
  Res := Run;
  if Res <> NIL then
    Res.Free;
end;

procedure TSQLiteQuery.Bind(Values: array of const);
var
  ParamI, I: Integer;
  ParamName: WideString;
  SkipParam: Boolean;
begin
  ParamI := 0;
  ParamName := '';

  for I := 0 to Length(Values) - 1 do
    with Values[I] do
      if (ParamName = '') and (VType = vtString) and (Copy(VPChar, 1, 1)[1] in [':', '?']) then
        ParamName := VPChar
        else
        begin
          SkipParam := False;

          if ParamName <> '' then
          begin
            if ParamName[1] = '?' then
              SkipParam := not HasParam( Copy(ParamName, 2, Length(ParamName)) );

            Delete(ParamName, 1, 1);
          end;

          if not SkipParam then
            case VType of
            vtInteger:    if ParamName = '' then BindIntegerTo(ParamI, VInteger)  else BindIntegerTo(ParamName, VInteger);
            vtExtended:   if ParamName = '' then BindDoubleTo(ParamI, VExtended^) else BindDoubleTo(ParamName, VExtended^);
            vtChar:       if ParamName = '' then BindTextTo(ParamI, VChar)        else BindTextTo(ParamName, VChar);
            vtString:     if ParamName = '' then BindBlobTo(ParamI, String(VPChar)) else BindBlobTo(ParamName, String(VPChar));
            vtWideString: if ParamName = '' then BindTextTo(ParamI, VPWideChar)   else BindTextTo(ParamName, VPWideChar);
            else
              if (VType = vtPointer) and (VPointer = NIL) then
                if ParamName = '' then
                  BindNullTo(ParamI)
                  else
                    BindNullTo(ParamName)
                else if ParamName = '' then
                  SQLiteRaise(FDatabase, ESQLiteUnsupportedBindArg.Create(ParamI, VType))
                  else
                    SQLiteRaise(FDatabase, ESQLiteUnsupportedBindArg.Create(ParamName, VType));
            end;

          Inc(ParamI);
          ParamName := '';
        end;
end;

procedure TSQLiteQuery.CheckBindRes(Param, Status: Integer);
begin
  if Status <> SQLITE_OK then
    SQLiteRaise(FDatabase, ESQLiteBind.Create(Status, Param));
end;

  procedure TSQLiteQuery.CheckBindRes(const Param: WideString; Status: Integer);
  begin
    if Status <> SQLITE_OK then
      SQLiteRaise(FDatabase, ESQLiteBind.Create(Status, Param));
  end;

procedure TSQLiteQuery.BindDoubleTo(Param: Integer; Value: Double);
begin
  CheckBindRes(Param, sqlite3_bind_double(FHandle, Param, Value));
end;

  procedure TSQLiteQuery.BindDoubleTo(const Param: WideString; Value: Double);
  begin
    CheckBindRes(Param, sqlite3_bind_double(FHandle, ParamIndex(Param), Value));
  end;

procedure TSQLiteQuery.BindIntegerTo(Param: Integer; Value: Int64);
begin
    CheckBindRes(Param, sqlite3_bind_int64(FHandle, Param, Value));
end;

  procedure TSQLiteQuery.BindIntegerTo(const Param: WideString; Value: Int64);
  begin
    CheckBindRes(Param, sqlite3_bind_int64(FHandle, ParamIndex(Param), Value));
  end;

procedure TSQLiteQuery.BindNullTo(Param: Integer);
begin
  CheckBindRes(Param, sqlite3_bind_null(FHandle, Param));
end;

  procedure TSQLiteQuery.BindNullTo(const Param: WideString);
  begin
      CheckBindRes(Param, sqlite3_bind_null(FHandle, ParamIndex(Param)));
  end;

procedure TSQLiteQuery.BindTextTo(Param: Integer; const Value: WideString);
begin
  CheckBindRes(Param, sqlite3_bind_text(FHandle, Param, ToSQLiteStr(Value), -1, SQLITE_TRANSIENT));
end;

  procedure TSQLiteQuery.BindTextTo(const Param: WideString; const Value: WideString);
  begin
    CheckBindRes(Param, sqlite3_bind_text(FHandle, ParamIndex(Param), ToSQLiteStr(Value), -1, SQLITE_TRANSIENT));
  end;

procedure TSQLiteQuery.BindBlobTo(Param: Integer; const Buf; Size: Integer);
begin
  CheckBindRes(Param, sqlite3_bind_blob(FHandle, Param, Buf, Size, SQLITE_TRANSIENT));
end;

  procedure TSQLiteQuery.BindBlobTo(const Param: WideString; const Buf; Size: Integer);
  begin
    CheckBindRes(Param, sqlite3_bind_blob(FHandle, ParamIndex(Param), Buf, Size, SQLITE_TRANSIENT));
  end;

procedure TSQLiteQuery.BindBlobTo(Param: Integer; Stream: TStream);
var
  S: String;
begin
  SetLength(S, Stream.Size - Stream.Position);
  Stream.ReadBuffer(S[1], Length(S));
  Stream.Position := Stream.Size - Length(S);

  BindBlobTo(Param, S);
end;

  procedure TSQLiteQuery.BindBlobTo(const Param: WideString; Stream: TStream);
  var
    S: String;
  begin
    SetLength(S, Stream.Size - Stream.Position);
    Stream.ReadBuffer(S[1], Length(S));
    Stream.Position := Stream.Size - Length(S);

    BindBlobTo(Param, S);
  end;

procedure TSQLiteQuery.BindBlobTo(Param: Integer; const Value: String);
begin
  BindBlobTo(Param, Value[1], Length(Value));
end;

  procedure TSQLiteQuery.BindBlobTo(const Param: WideString; const Value: String);
  begin
    BindBlobTo(Param, Value[1], Length(Value));
  end;

procedure TSQLiteQuery.BindZeroBlobTo(Param, Size: Integer);
begin
  CheckBindRes(Param, sqlite3_bind_zeroblob(FHandle, Param, Size));
end;

  procedure TSQLiteQuery.BindZeroBlobTo(const Param: WideString; Size: Integer);
  begin
    CheckBindRes(Param, sqlite3_bind_zeroblob(FHandle, ParamIndex(Param), Size));
  end;

function TSQLiteQuery.ParamName(Index: Integer): WideString;
begin
  Result := FromSQLiteStr( sqlite3_bind_parameter_name(FHandle, Index) );
end;

  function TSQLiteQuery.ParamIndex(const Param: WideString): Integer;
  begin
    Result := sqlite3_bind_parameter_index(FHandle, ToSQLiteStr(Param));
  end;

function TSQLiteQuery.HasParam(const Param: WideString): Boolean;
begin
  Result := ParamIndex(Param) > 0;
end;

function TSQLiteQuery.HasParam(Param: Integer): Boolean;
begin
  Result := ParamName(Param) <> '';
end;

function TSQLiteQuery.ParamCount: Integer;
begin
  Result := sqlite3_bind_parameter_count(FHandle);
end;

{ TSQLiteResult }

constructor TSQLiteResult.Create(Handle: Pointer; DB: TSQLiteDatabase = NIL);
begin
  FHandle := Handle;
  FHasAny := True;
  FDatabase := DB;

  ResetRow;
end;

destructor TSQLiteResult.Destroy;
begin
  Close;
  inherited;
end;

procedure TSQLiteResult.ResetRow;
begin
  ZeroMemory(@FSetCols, SizeOf(FSetCols));
end;

procedure TSQLiteResult.Close;
begin
  sqlite3_finalize(FHandle);
  FHandle := NIL;
end;

function TSQLiteResult.ColCount: Integer;
begin
  if HasAny then
    Result := sqlite3_data_count(FHandle)
    else
      Result := 0;
end;

function TSQLiteResult.Column(const Name: WideString): TSQLiteColumn;
var
  I: Integer;
  Names: WideString;
begin
  if not HasAny then
    SQLiteRaise(FDatabase, ESQLiteNoMoreResults.Create('Column(Name)'));

  for I := 0 to ColCount - 1 do
    if Column(I).Name = Name then
    begin
      Result := Column(I);
      Exit;
    end;

  Names := '';
  for I := 0 to ColCount - 1 do
    Names := Names + ', ' + Column(I).Name;

  SQLiteRaise(FDatabase, ESQLiteNoColumnByName.Create( Name, Copy(Names, 3, Length(Names)) ));
end;

function TSQLiteResult.Column(Index: Integer): TSQLiteColumn;
begin
  if not HasAny then
    SQLiteRaise(FDatabase, ESQLiteNoMoreResults.Create('Column(Index)'));

  if (Index < 0) or (Index >= Length(FColumns)) or (Index >= ColCount) then
    SQLiteRaise(FDatabase, ESQLiteNoColumnByIndex.Create(Index));

  if not FSetCols[Index] then
  begin
    FColumns[Index] := FetchColumn(Index);
    FSetCols[Index] := True;
  end;

  Result := FColumns[Index];
end;

  function TSQLiteResult.FetchColumn(Index: Integer): TSQLiteColumn;
  var
    Ptr: Pointer;
  begin
    with Result do
    begin
      ZeroMemory(@Result, SIzeOf(Result));

      Kind := SQLiteType( sqlite3_column_type(FHandle, Index) );
      DeclType := FromSQLiteStr( sqlite3_column_decltype(FHandle, Index) );

      Name := FromSQLiteStr( sqlite3_column_name(FHandle, Index) );
      Origin := FromSQLiteStr( sqlite3_column_origin_name(FHandle, Index) );

      case Kind of
      sqInteger:
        begin
          Int64 := sqlite3_column_int64(FHandle, Index);
          Int := Int64;
        end;

      sqFloat:
        Float := sqlite3_column_double(FHandle, Index);

      sqBlob, sqText:
        begin
          SetLength(Blob, sqlite3_column_bytes(FHandle, Index));

          Ptr := sqlite3_column_blob(FHandle, Index);
          Move(Ptr^, Blob[1], Length(Blob));

          if Kind = sqText then
          begin
            Text := UTF8Decode(Blob);
            Blob := '';
          end;
        end;
      end;
    end;
  end;

function TSQLiteResult.Next: Boolean;
var
  Error: Integer;
begin
  if FHasAny then
  begin
    Error := sqlite3_step(FHandle);
    if (Error = SQLITE_OK) or (Error = SQLITE_DONE) then
      FHasAny := False
      else if Error <> SQLITE_ROW then
        SQLiteRaise(FDatabase, ESQLiteSteppingSQL.Create(Error));

    ResetRow;
  end;

  Result := FHasAny;
end;

procedure TSQLiteResult.Reset;
var
  Error: Integer;
begin
  Error := sqlite3_reset(FHandle);

  if (Error <> SQLITE_OK) and (Error <> SQLITE_DONE) and (Error <> SQLITE_ROW) then
    SQLiteRaise(FDatabase, ESQLiteResettingResult.Create(Error))
    else
    begin
      FHasAny := True;
      FHasAny := Next;
    end;
end;

function TSQLiteResult.ToRecord(var Rec; Fields: array of const): Integer;
var
  I, Size: Integer;
  Col: TSQLiteColumn;
  Value: array[0..7] of Byte;
begin
  Result := 0;

  I := 0;
  while I < Length(Fields) do
  begin
    if Fields[I].VType = vtExtended then
      Size := Trunc(Fields[I].VExtended^)
      else
      begin
        { Fields[I] = int/str; [I + 1] = Size (opt.) }

        case Fields[I].VType of
        vtInteger:      Col := Column(Fields[I ].VInteger);
        vtAnsiString:   Col := Column(String( Fields[I ].VPChar ));
        else
          SQLiteRaise(FDatabase, ESQLiteResultToRecord.Create('type of <col> item is neither vtInteger nor vtPChar'));
        end;

        Size := 4;
        if Fields[I + 1].VType = vtInteger then
        begin
          Size := Fields[I + 1].VInteger;
          Inc(I);
        end;

        Int64((@Value[0])^) := 0;

        case Col.Kind of
        sqFloat:
          Move(Col.Float, Value[0], SizeOf(Col.Float));
        sqInteger:
          if Size > SizeOf(Col.Int) then
            Move(Col.Int64, Value[0], SizeOf(Col.Int64))
            else
              Move(Col.Int, Value[0], SizeOf(Col.Int));
        sqText, sqBlob:
          if Size <> 4 then
            SQLiteRaise(FDatabase, ESQLiteResultToRecord.Create('expected an sqBlob/sqText field to be of Size 4'))
            else if Col.Kind = sqText then
              PWideString( @TRec(Rec)[Result] )^ := Col.Text
              else if Col.Kind = sqBlob then
                PString( @TRec(Rec)[Result] )^ := Col.Blob;
        end;

        if not (Col.Kind in [sqText, sqBlob]) then
          Move(Value[0], TRec(Rec)[Result], Size);
      end;

    Inc(Result, Size);
    Inc(I);
  end;

  if I <> Length(Fields) then
    SQLiteRaise(FDatabase, ESQLiteResultToRecord.Create('wrong Fields length'));
end;

function TSQLiteResult.ToRecords(var RecArray; MaxCount: Integer; Fields: array of const;
  TailSpace: Integer = 0): Integer;
var
  Ptr: Integer;
begin
  if TailSpace < 0 then
    SQLiteRaise(FDatabase, ESQLiteResultToRecord.Create('negative TailSpace'));

  Result := 0;

    Ptr := 0;
    repeat
      if Result >= MaxCount then
        Break;

      Inc( Ptr, ToRecord(TRec(RecArray)[Ptr], Fields) + TailSpace );
      Inc(Result);
    until not Next;

  if Result >= MaxCount then
    Result := -1;
end;

function TSQLiteResult.RowCount: Integer;
begin
  Reset;

  Result := 0;
  if HasAny then
    repeat
      Inc(Result);
    until not Next;

  Reset;
end;

{ TSQLiteEmptyResult }

constructor TSQLiteEmptyResult.Create;
begin
  inherited Create(NIL);
  FHasAny := False;
end;

{ TSQLiteBlob }

constructor TSQLiteBlob.Acquire(Handle: Pointer);
begin
  FHandle := Handle;
end;

destructor TSQLiteBlob.Destroy;
begin
  sqlite3_blob_close(FHandle);

  if FTable <> NIL then
    FTable.FBlobs.Extract(Self);

  inherited;
end;

procedure TSQLiteBlob.Reopen(RowID: Int64);
begin
  Check('reopen', sqlite3_blob_reopen(FHandle, RowID));
end;

function TSQLiteBlob.GetSize: Int64;
begin
  Result := sqlite3_blob_bytes(FHandle);
end;

procedure TSQLiteBlob.SetSize(NewSize: Integer);
begin
  SQLiteRaise(FDatabase, ESQLiteSettingBlobSize.Create(GetSize, NewSize));
end;

function TSQLiteBlob.Seek(Offset: Integer; Origin: Word): LongInt;
begin
  case Origin of
  soFromBeginning:  FPosition := Offset;
  soFromCurrent:    Inc(FPosition, Offset);
  soFromEnd:        FPosition := GetSize + Offset;
  end;

  if FPosition < 0 then
    FPosition := 0;
  if FPosition >= GetSize then
    FPosition := GetSize;

  Result := FPosition;
end;

function TSQLiteBlob.Read(var Buffer; Count: Integer): Longint;
begin
  Check('read', sqlite3_blob_read(FHandle, Buffer, Count, FPosition));
  Result := Count;
end;

function TSQLiteBlob.Write(const Buffer; Count: Integer): Longint;
begin
  Check('write', sqlite3_blob_write(FHandle, Buffer, Count, FPosition));
  Result := Count;
end;

procedure TSQLiteBlob.Check(Operation: String; Status: Integer);
begin
  if Status <> SQLITE_OK then
    SQLiteRaise(FDatabase, ESQLiteBlobOperation.Create(Status, Operation));
end;

{ TSQLiteObjectList }

constructor TSQLiteObjectList.Create;
begin
  inherited Create(True);
end;

procedure TSQLiteObjectList.Delete(ObjInst: TObject);
var
  I: Integer;
begin
  I := IndexOf(ObjInst);
  if I <> -1 then
    Delete(I);
end;

end.
