{
Classes to JavaScript and Ext JS translating from Object Pascal.
Associates semantic concepts of JavaScript and Ext JS for Object Pascal, such as: Function, Object, List of Objects and Ajax Method.
It's the heart of the automatic translation method from Object Pascal to JavaScript, that I call "Self-translating".
It takes advantage of the fact that JavaScript and Object Pascal are structurally similar languages,
where there is almost one to one parity between its syntax and semantic structures.
-ExtPascal is composed of four main components:-
1. The <color red>Parser</color> (<link ExtToPascal.dpr>) able to scan Ext JS documentation in HTML format and to create the <color red>Wrapper</color>.
2. The <color red>Wrapper</color> programmatically created by Parser. It's in fact a set of units (twelve in Ext JS 2.1), which has the definition of all Ext JS classes, properties, methods and events.
3. The <color red>Self-translating</color> engine, <link ExtPascal.pas, ExtPascal unit>. It's triggered when using the <color red>Wrapper</color>, ie by creating objects, assigning properties and events, and invoking methods.
4. The <link FCGIApp.pas, FastCGI> multithread environment. It implements FastCGI protocol using TCP/IP Sockets and statefull, keep-alive, multithread web application behavior.
<image extpascal>
1. The <color red>Parser</color> reads the HTML documentation of Ext JS,
2. Reads ExtFixes.txt file to fix documentation faults and omissions, and
3. Generates the <color red>Wrapper</color>.
4. With the application running, a browser session does a HTTP request to the Web Server.
5. The Web Server does a FastCGI request to the application that creates a <color red>thread</color> to handle the request.
6. The <color red>thread</color> creates ExtObjects, set properties and call methods from <color red>Wrapper</color> Units.
7. For each these tasks the <color red>Self-translating</color> is invoked
8. Generating JavaScript code that uses Ext JS classes.
9. At end of request processing, the <color red>thread</color> reads and formats all JS generated
10. And sends the response to browser session. New requests can be done begining from step 4.
So the translating is not focused on JavaScript language, but to access widget frameworks made in JavaScript.
In this way the use of (X)HTML, JavaScript and CSS is minimum.
Indeed the <color red>Parser</color> can be adapted to read the documentation of another JavaScript framework, Dojo for example.

ExtPascal has one optional fifth component the <link CGIGateway.dpr>.

Author: Wanderlan Santos dos Anjos, wanderlan.anjos@gmail.com
Date: jun-2008
License: <extlink http://www.opensource.org/licenses/bsd-license.php>BSD</extlink>
}

unit ExtPascal;

//@@Overview
//<copy ExtPascal.pas>

// Enabling SERVICE directive ExtPascal application can be used as a Windows Service,
// In command line use -INSTALL to install the service and - UNINSTALL to uninstall the service
{$IFDEF MSWINDOWS}
{.$DEFINE SERVICE}
{$ENDIF}

// Enabling WebServer directive ExtPascal can be used within an embedded Indy based WebServer
{$IFNDEF SERVICE}
{.$DEFINE WebServer}
{$ENDIF}

// Uses ext-all-debug.js, format all JS/CSS source code to facilitate debugging on browser and locates line error if using Firefox on AJAX responses
{.$DEFINE DEBUGJS}

// Uses CacheFly for performance boost see: http://extjs.com/blog/2008/11/18/ext-cdn-custom-builds-compression-and-fast-performance/
{.$DEFINE CacheFly}

// Uses DebugExtJS to load ext-all-debug.js library instead ext-all.js to debugging purposes
{.$DEFINE DebugExtJS}

interface

uses
  {$IFNDEF WebServer}FCGIApp{$ELSE}IdExtHTTPServer{$ENDIF},
  Classes, ExtPascalUtils;

type
  TArrayOfString  = array of string;
  TArrayOfInteger = array of Integer;
  TExtObjectList  = class;
  TExtFunction    = class;

  {
  Defines an user session opened in a browser. Each session is a FastCGI thread that owns additional JavaScript and Ext JS resources
  as: theme, charset, language, Ajax, error messages using Ext look, JS libraries and CSS.
  The <color red>"Self-translating"</color> is implemented in this class in <link TExtObject.JSCode, JSCode> method.
  }
  TExtThread = class(TWebSession)
  private
    Style, Libraries, CustomJS, FLanguage : string;
    JSReturns : TStringList;
    Sequence  : cardinal;
    procedure RelocateVar(JS, JSName : string; I : integer);
    function GetStyle: string;
  protected
    procedure RemoveJS(const JS : string);
    function BeforeHandleRequest : boolean; override;
    procedure AfterHandleRequest; override;
    procedure AfterNewSession; override;
    {$IFDEF HAS_CONFIG}
    procedure DoReconfig; override;
    procedure ReadConfig;
    {$ENDIF}
    function GarbageFixName(const Name: string): string; override;
    procedure OnError(const Msg, Method, Params : string); override;
    function GetSequence : string;
    function GetUrlHandlerObject: TObject; override;
    function JSConcat(PrevCommand, NextCommand : string) : string;
  public
    HTMLQuirksMode : boolean; // Defines the (X)HTML DocType. True to Transitional (Quirks mode) or false to Strict. Default is false.
    Theme     : string; // Sets or gets Ext JS installed theme, default '' that is Ext Blue theme
    ExtPath   : string; // Installation path of Ext JS framework, below the your Web server document root. Default value is '/ext'
    ImagePath : string; // Image path below ExtPath, used by <link TExtThread.SetIconCls, SetIconCls> method. Default value is '/images'
    ExtBuild  : string; // Custom <extlink http://www.extjs.com/products/extjs/build/>ExtJS build</extlink>. Default is ext-all.
    property Language : string read FLanguage write FLanguage; // Actual language for this session, reads HTTP_ACCEPT_LANGUAGE header
    constructor Create(AOwner: TObject); override;
    procedure InitDefaultValues; override;
    procedure JSCode(JS : string; JSClassName : string = ''; JSName : string = ''; Owner : string = '');
    procedure JSSleep(MiliSeconds : integer);
    procedure SetStyle(pStyle : string = '');
    procedure SetLibrary(pLibrary : string = ''; CSS : boolean = false; HasDebug : boolean = false; DisableExistenceCheck : boolean = false);
    procedure SetCSS(pCSS : string; Check : boolean = true);
    procedure SetIconCls(Cls : array of string);
    procedure SetCustomJS(JS : string = '');
    procedure ErrorMessage(Msg : string; Action : string = ''); overload;
    procedure ErrorMessage(Msg : string; Action : TExtFunction); overload;
    procedure Alert(const Msg : string); override;
  published
    procedure HandleEvent; virtual;
  end;

  {$M+}
  {
  Ancestor of all classes and components of Ext JS framework.
  Each TExtObject has the capability to self-translate to JavaScript during the program execution.
  When a property is assigned or a method is called the <link TExtThread.JSCode, Self-translating> enter in action
  translating these Object Pascal commands to JavaScript.
  }
  TExtObject = class
  private
    function  WriteFunction(Command : string): string;
    function  GetJSCommand : string;
    procedure SetJSCommand(const Value : string);
    function  PopJSCommand : string;
    function FormatParams(MethodName : string; Params : array of const): string;
    procedure AjaxCode(MethodName, RawParams : string; Params : array of const);
    function Ajax(Method : TExtProcedure; Params : string) : TExtFunction; overload;
    function AddJSReturn(Expression : string; MethodsValues : array of const): string;
    function FindMethod(Method : TExtProcedure; var PascalName, ObjName : string) : TExtFunction;
    function GetDownloadJS(Method: TExtProcedure; Params: array of const): string;
  protected
    FJSName    : string;  // Internal JavaScript name generated automatically by <link TExtObject.CreateJSName, CreateJSName>
    Created    : boolean; // Tests if object already created
    FJSCommand : string;
    function ConfigAvailable(JSName : string) : boolean;
    function ExtractJSCommand : string;
    function IsParent(CName : string): boolean;
    function VarToJSON(A : array of const)     : string; overload;
    function VarToJSON(Exts : TExtObjectList)  : string; overload;
    function ArrayToJSON(Strs : {$IF Defined(FPC) or (RTLVersion < 20)}TArrayOfString{$ELSE}array of string{$IFEND}) : string; overload;
    function ArrayToJSON(Ints : {$IF Defined(FPC) or (RTLVersion < 20)}TArrayOfInteger{$ELSE}array of integer{$IFEND}) : string; overload;
    function ParamAsInteger(ParamName : string) : integer;
    function ParamAsDouble(ParamName : string) : double;
    function ParamAsBoolean(ParamName : string) : boolean;
    function ParamAsString(ParamName : string) : string;
    function ParamAsTDateTime(ParamName : string) : TDateTime;
    function ParamAsObject(ParamName : string) : TExtObject;
    procedure CreateVar(JS : string);
    procedure CreateVarAlt(JS : string);
    procedure CreateJSName;
    procedure InitDefaults; virtual;
    procedure HandleEvent(const AEvtName : string); virtual;
    property JSCommand : string read GetJSCommand write SetJSCommand; // Last commands written in Response
  public
    IsChild : boolean;
    constructor CreateInternal(Owner : TExtObject; Attribute : string);
    constructor Create(Owner : TExtObject = nil);
    constructor CreateSingleton(Attribute : string = '');
    constructor AddTo(List : TExtObjectList);
    constructor Init(Method : TExtFunction); overload;
    constructor Init(Command : string); overload;
    destructor Destroy; override;
    function DestroyJS : TExtFunction; virtual;
    procedure Free(CallDestroyJS : boolean = false);
    procedure Delete;
    procedure DeleteFromGarbage;
    function JSClassName : string; virtual;
    function JSArray(JSON : string; SquareBracket : boolean = true) : TExtObjectList;
    function JSObject(JSON : string; ObjectConstructor : string = ''; CurlyBracket : boolean = true) : TExtObject;
    function JSFunction(Params, Body : string) : TExtFunction; overload;
    procedure JSFunction(Name, Params, Body : string); overload;
    function JSFunction(Body : string) : TExtFunction; overload;
    function JSFunction(Method : TExtProcedure; Silent : boolean = false) : TExtFunction; overload;
    function JSExpression(Expression : string; MethodsValues : array of const) : integer; overload;
    function JSExpression(Method : TExtFunction) : integer; overload;
    function JSString(Expression : string; MethodsValues : array of const) : string; overload;
    function JSString(Method : TExtFunction) : string; overload;
    function JSMethod(Method : TExtFunction) : string;
    procedure JSCode(JS : string; pJSName : string = ''; pOwner : string = '');
    procedure JSSleep(MiliSeconds: integer);
    function Ajax(MethodName : string; Params : array of const; IsEvent : boolean = false) : TExtFunction; overload;
    function Ajax(Method : TExtProcedure) : TExtFunction; overload;
    function Ajax(Method : TExtProcedure; Params : array of const) : TExtFunction; overload;
    function AjaxExtFunction(Method : TExtProcedure; Params : array of TExtFunction) : TExtFunction; overload;
    function AjaxSelection(Method : TExtProcedure; SelectionModel : TExtObject; Attributes, TargetQueries : string; Params : array of const) : TExtFunction;
    function AjaxForms(Method : TExtProcedure; Forms : array of TExtObject) : TExtFunction;
    function RequestDownload(Method : TExtProcedure) : TExtFunction; overload;
    function RequestDownload(Method : TExtProcedure; Params : array of const) : TExtFunction; overload;
    procedure Download(Method : TExtProcedure); overload;
    procedure Download(Method : TExtProcedure; Params : array of const); overload;
    function MethodURI(Method : TExtProcedure; Params : array of const) : string; overload;
    function MethodURI(Method : TExtProcedure) : string; overload;
    function MethodURI(MethodName : string; Params : array of const) : string; overload;
    function MethodURI(MethodName : string) : string; overload;
    function CharsToPixels(Chars : integer) : integer;
    function LinesToPixels(Lines : integer) : integer;
    property JSName : string read FJSName; // JS variable name to this object, it's created automatically when the object is created
  end;
  {$M-}
  TExtObjectClass = class of TExtObject;

  {
  Basic class descending of TExtObject that defines a JavaScript function. All functions and procedures of Ext JS framework are converted to Pascal functions
  where its return is this class. With this all converted functions by <link ExtPascal.pas, Wrapper> could be assigned to event handlers.
  }
  TExtFunction = class(TExtObject);

  // List of TExtObjects. The <link ExtPascal.pas, Wrapper> convey the JavaScript Array type to this class
  TExtObjectList = class(TExtFunction)
  private
    FObjects : array of TExtObject;
    Attribute, JSName : string;
    Owner : TExtObject;
    function GetFObjects(I : integer) : TExtObject;
  public
    property Objects[I : integer] : TExtObject read GetFObjects; default; // Returns the Ith object in the list, start with 0.
    constructor CreateSingleton(pAttribute : string);
    constructor Create(pOwner : TExtObject = nil; pAttribute : string = '');
    destructor Destroy; override;
    procedure Add(Obj : TExtObject);
    procedure Remove(Obj : TExtObject);
    function IndexOf(Obj : TExtObject): Integer;
    function Count : integer;
  end;

//(*DOM-IGNORE-BEGIN
  {
  Classes that can not be documented.
  They are usually JS basic classes without reference in Ext JS documentation by omission or fault.
  }
  THTMLElement = class(TExtObject);
  TStyleSheet = class(TExtObject);
  TRegExp = type string;
  TCSSRule = class(TExtObject);
  TXMLDocument = class(TExtObject);
  TNodeList = class(TExtObjectList);
  TExtDataNode = class(TExtObject);
  TRegion = type string;
  TNativeMenu = TExtObject;
  Tel = type string; // doc fault
  TEvent = class(TExtObject);
  TEventObject = TEvent;
  TExtEventObject = TEventObject;
  THTMLNode = TExtObject;
  TConstructor = class(TExtObject);
  TExtLibRegion = class(TExtObject); //doc fault
  TvisMode = Integer; // doc fault
  TThe = TExtObject; // doc fault
  TThis = TExtObject; // doc fault
  TairNativeMenu = TExtObject;
  TX = TExtObject; // doc fault
  TN1 = TExtObject; // doc fault
  TN2 = TExtObject; // doc fault
  TLayout = TExtObject; // Poor hierarchy definition
  TId = TExtObject; // doc fault
  TiPageX = TExtObject; // doc fault
  TiPageY = TExtObject; // doc fault
  TExtGridGrid = TExtObject; // doc fault
  TTreeSelectionModel = TExtObject; // doc fault
  TSelectionModel = TExtObject; // doc fault
  TDataSource = TExtObject; // doc fault
  TAirNotificationType = TExtObject; // doc fault
  TIterable = TExtObjectList; // doc fault
  TAnything = TExtObject; // doc fault
  TNodeLists = class(TExtObjectList); // doc fault
  TArrays = TExtObjectList; // doc fault
  TExtDirectExceptionEvent = TEvent; // doc fault Ext 3.0
  TExtDirectEvent = TEvent; // doc fault Ext 3.0
  TExtDirectTransaction = TExtObject; // doc fault Ext 3.0
  TDOMElement = TExtObject; // doc fault Ext 3.0
  TRecord = TExtObject; // Ext 3.0 RC2
  TNull = TExtObject; // Ext 3.0 RC2
  TMisc = TExtObject; // doc fault Ext 3.0
  THash = TExtObject; // doc fault Ext 3.1
  TXMLElement = TExtObject; // doc fault Ext 3.1
  TExtListView = TExtObject; // doc fault Ext 3.1
  TExtSlider = TExtObject; // doc fault Ext 3.2
//DOM-IGNORE-END*)

const
  DeclareJS    = '/*var*/ '; // Declare JS objects as global
  CommandDelim = #3;         // Internal JS command delimiter
  IdentDelim   = #4;         // Internal JS identifier delimiter
  JSDelim      = #5;         // Internal JSCommand delimiter

implementation

uses
  {$IFDEF MSWINDOWS}{$IF RTLVersion <= 21}Windows,{$IFEND}{$ENDIF}
  SysUtils, StrUtils, Math, Ext, ExtUtil, ExtGrid, ExtForm;

var
  _JSFormatSettings: TFormatSettings;

threadvar
  InJSFunction : boolean;

{ TExtThread }

{
Removes identificated JS commands from response.
Used internally by Self-Translating mechanism to repositioning JS commands.
@param JS JS command with sequence identifier.
@see TExtObject.ExtractJSCommand
}
procedure TExtThread.RemoveJS(const JS : string);
var
  I : integer;
begin
  I := ExtPascalUtils.RPosEx(JS, Response, 1);
  if I <> 0 then delete(Response, I, length(JS))
end;

{
Adds/Removes an user JS library to be used in current response.
If the WebServer is Apache tests if the library exists.
Repeated libraries are ignored.
@param pLibrary JS library without extension (.js), but with Path based on Web server document root.
@param CSS pLibrary has a companion stylesheet (.css) with same path and name.
@param HasDebug Library has a debug, non minified, version. Default is false.
If pLibrary is '' then all user JS libraries to this session will be removed from response.
@example <code>SetLibrary('');</code>
@example <code>SetLibrary(<link ExtPath> + '/examples/tabs/TabCloseMenu');</code>
}
procedure TExtThread.SetLibrary(pLibrary : string = ''; CSS : boolean = false; HasDebug : boolean = false;
  DisableExistenceCheck : boolean = false);
var
  Root : string;
begin
  if pos(pLibrary + '.js', Libraries) = 0 then
    if pLibrary = '' then
      Libraries := '' // Clear Libraries
    else begin
      if DisableExistenceCheck then
        Root := ''
      else
        Root := RequestHeader['DOCUMENT_ROOT'];
      if (Root = '') or ((Root <> '') and FileExists(Root + pLibrary + '.js')) then begin
        Libraries := Libraries + '<script src="' + pLibrary{$IFDEF DEBUGJS}+ IfThen(HasDebug, '-debug', ''){$ENDIF} + '.js"></script>'^M^J;
        if CSS then begin
          if not DisableExistenceCheck and not FileExists(Root + pLibrary + '.css') then // Assume in /css like ux
            pLibrary := ExtractFilePath(pLibrary) + 'css/' + ExtractFileName(pLibrary);
          Libraries := Libraries + '<link rel=stylesheet href="' + pLibrary + '.css" />';
        end;
      end
      else
        raise Exception.Create('Library: ' + Root + pLibrary + '.js not found');
    end;
end;

{
Adds/Removes an user CSS (cascade style sheet) to be used in current response.
If the WebServer is Apache tests if the CSS file exists.
Repeated CSS's are ignored.
@param pCSS CSS file name without extension (.css), but with Path based on Web server document root.
@param Check Checks if the CSS file exists, default is true.
If pCSS is '' then all user CSS AND JS libraries to this session will be removed from response.
}
procedure TExtThread.SetCSS(pCSS : string; Check : boolean = true);
var
  Root : string;
begin
  if pos(pCSS + '.css', Libraries) = 0 then
    if pCSS = '' then
      Libraries := '' // Clear Libraries
    else begin
      Root := RequestHeader['DOCUMENT_ROOT'];
      if Check and (Root <> '') and not FileExists(Root + pCSS + '.css') then
        raise Exception.Create('Stylesheet: ' + Root + pCSS + '.css not found')
      else
        Libraries := Libraries + '<link rel=stylesheet href="' + pCSS + '.css" />';
    end;
end;
{
Adds/Removes an user JS code to be used in current response.
Repeated code is ignored.
@param JS JS code to inject in response. If JS is '' then all user JS code to this session will be removed from response.
}
procedure TExtThread.SetCustomJS(JS : string = ''); begin
  if pos(JS, CustomJS) = 0 then
    if JS = '' then
      CustomJS := ''
    else
      CustomJS := CustomJS + JS;
end;

{
Creates CSS classes to use with IconCls properties in Buttons. The images are .png files in 16x16 format.
Set <link ImagePath> variable to appropriate value before to use this method. Default is /images.
If the WebServer is Apache tests if each image exists.
Repeated images are ignored.
@param Cls String array with the names of .png files.
@example <code>
SetIconCls(['task', 'objects', 'commit', 'cancel', 'refresh', 'info', 'help', 'pitinnu', 'exit']);
with TExtToolbarButton.AddTo(Items) do begin
  Text    := 'Tasks';
  IconCls := 'task';
  Menu    := TaskMenu;
end;</code>
}
procedure TExtThread.SetIconCls(Cls : array of string);
var
  I : integer;
  Root : string;
begin
  Root := RequestHeader['Document_Root'];
  for I := 0 to high(Cls) do
    if (Root = '') or ((Root <> '') and FileExists(Root + ImagePath + '/' + Cls[I] + '.png')) then
      SetStyle('.' + Cls[I] + '{background-image:url("' + ImagePath + '/' + Cls[I] + '.png") !important}')
    else
      raise Exception.Create('Image file: ' + Root + ImagePath + '/' + Cls[I] + '.png not found');
end;

(*
Adds/Removes a user stylesheet to be used in current response.
Repeated style is ignored.
@param pStyle Styles to apply upon HTML or Ext elements in this response using CSS notation.
If pStyle is '' then all user styles to this session will be removed from response.
@example <code>SetStyle('');</code>
@example <code>SetStyle('img:hover{border:1px solid blue}');</code>
*)
procedure TExtThread.SetStyle(pStyle : string); begin
  if pos(pStyle, Style) = 0 then
    if pStyle = '' then
      Style := ''
    else
      Style := Style + pStyle
end;

// Returns all styles in use in current response
function TExtThread.GetStyle : string; begin
  if Style = '' then
    Result := ''
  else
    Result := '<style>' + {$IFDEF DEBUGJS}BeautifyCSS(Style){$ELSE}Style{$ENDIF} + '</style>'^M^J;
end;

// Returns a object which will be used to handle the page method. We will call it's published method based on PathInfo.
function TExtThread.GetUrlHandlerObject: TObject; begin
  if (Query['Obj'] = '') or (Query['IsEvent'] = '1') then
    Result := inherited GetUrlHandlerObject
  else
    Result := GarbageFind(Query['Obj']);
end;

{
Shows an error message in browser session using Ext JS style.
@param Msg Message text, can to use HTML to formating text.
@param Action Optional action that will be executed after user to click Ok button. Could be JavaScript or ExtPascal commands
@example <code>ErrorMessage('User not found.');</code>
@example <code>ErrorMessage('Context not found.<br/>This Window will be reloaded to fix this issue.', 'window.location.reload()');</code>
}
procedure TExtThread.ErrorMessage(Msg : string; Action : string = ''); begin
  JSCode('Ext.Msg.show({title:"Error",msg:' + StrToJS(Msg, true) + ',icon:Ext.Msg.ERROR,buttons:Ext.Msg.OK' +
    IfThen(Action = '', '', ',fn:function(){' + Action + '}') + '});');
end;

{
Shows an error message in browser session using Ext JS style.
@param Msg Message text, can to use HTML to formating text.
@param Action Optional action that will be executed after user to click Ok button. Could be JavaScript or ExtPascal commands
@example <code>ErrorMessage('Illegal operation.<br/>Click OK to Shutdown.', Ajax(Shutdown));</code>
}
procedure TExtThread.ErrorMessage(Msg : string; Action : TExtFunction); begin
  ErrorMessage(Msg, Action.ExtractJSCommand);
end;

function TExtThread.GarbageFixName(const Name: string): string; begin
  Result := AnsiReplaceStr(Name, IdentDelim, '');
end;

{
Occurs when an exception is raised during the execution of method that handles the request (PATH_INFO).
Display error message with exception message, method name and method params.
@param Msg Exception message
@param Method Method name invoked by Browser (PATH_INFO) or thru AJAX request
@param Params Method params list
@exception TAccessViolation If current request is AJAX the session can be fixed reloading the page.
}
procedure TExtThread.OnError(const Msg, Method, Params : string);
var
  FMsg: string;
begin
  Response := '';
  FMsg := Msg;
  if IsAjax and (pos('Access violation', FMsg) <> 0) then
    FMsg := FMsg + '<br/><b>Reloading this page (F5) perhaps fix this error.</b>';
  ErrorMessage(FMsg + '<br/>Method: ' + IfThen(Method = '', 'Home', Method) + IfThen(Params = '', '', '<br/>Params:<br/>' + AnsiReplaceStr(Params, '&', '<br/>')));
end;

procedure TExtThread.Alert(const Msg : string); begin
  ErrorMessage(Msg)
end;

{
Self-translating main procedure. Translates Object Pascal code to JavaScript code.
Self-translating (ST) is not a compiler. It's a minimalist (very small, ultra quick and dirty) approach that imitates an online interpreter.
You code in Object Pascal and when the program runs it automatically generates the corresponding JavaScript code.

But there is an essential limitation; it does not create business rules or sophisticated behavior in JavaScript.
So it does not interpret "IF", "WHILE", "CASE", "TRY", etc commands, but "IF", "WHILE", etc realizes a conditional code generation
on Server side as ASP and JSP does it. ST is used to create objects and widgets, to set properties and events and to call methods.
It's analogous to Delphi .dfm file role: to describe a GUI.
There are additional facilities to invoke complex Server side logic using <link TExtObject.Ajax, AJAX>, to define small <link TExtObject.JSFunction, functions> in JavaScript and
to use <link TExtThread.SetLibrary, large JS libraries>. It's enough to create powerful GUIs.
The rest (business rules, database access, etc) should be done in Object Pascal on Server side.

Basic work:
* JS commands are appended to Response.
* JS attributes are found in Response using yours JSName attribute and setted in place.
* If not found, JS attributes are appended to Response.
@param JS JS commands or assigning of attributes or events
@param JSName Optional current JS object name
@param Owner Optional JS object owner for TExtObjectList
}
procedure TExtThread.JSCode(JS : string; JSClassName : string = ''; JSName : string = ''; Owner : string = '');
var
  I, J : integer;
begin
  if JS <> '' then begin
    if JS[length(JS)] = ';' then begin // Command
      I := pos('.', JS);
      J := pos(IdentDelim, JS);
      if (pos('Singleton', JSClassName) = 0) and (J > 0) and (J < I) and (pos(DeclareJS, JS) = 0) and not GarbageExists(copy(JS, J-1, I-J+1)) then
        raise Exception.Create('Public property or Method: ''' + JSClassName + '.' + copy(JS, I+1, FirstDelimiter('=(', JS, I)-I-1) + ''' requires explicit ''var'' declaration.');
      I := length(Response) + 1
    end
    else  // set attribute
      if JSName = '' then
        raise Exception.Create('Missing '';'' in command: ' + JS)
      else begin
        I := pos('/*' + JSName + '*/', Response);
        if I = 0 then
          raise Exception.Create('Config Option: ' + JS + '<br/>is refering a previous request,' +
            '<br/>it''s not allowed in AJAX request or JS handler.<br/>Use equivalent Public Property or Method instead.');
        if not CharInSet(Response[I-1], ['{', '[', '(', ';']) then JS := ',' + JS;
      end;
    insert(JS, Response, I);
    if (pos('O' + IdentDelim, JS) <> 0) and (pos('O' + IdentDelim, JSName) <> 0) then begin
      if Owner <> '' then JSName := Owner;
      RelocateVar(JS, JSName, I+length(JS));
    end;
  end;
end;

{
Forces that the JS Object declaration (var) occurs before of its use: method call, get/set attributes, etc,
relocating the declaration to a previous position in Response.
@param JS Command or attribute that uses the JS Object.
@param JSName Current JS Object.
@param I Position in Response string where the JS command ends.
}
procedure TExtThread.RelocateVar(JS, JSName : string; I : integer);
var
  VarName, VarBody : string;
  J, K : integer;
begin
  J := LastDelimiter(':,', JS);
  if J <> 0 then
    VarName := copy(JS, J+1, posex(IdentDelim, JS, J+3)-J)
  else
    VarName := JS;
  J := posex('/*' + VarName + '*/', Response, I);
  if J > I then begin
    K := pos('/*' + JSName + '*/', Response);
    K := posex(';', Response, K)+1;
    J := posex(';', Response, J);
    VarBody := copy(Response, K, J-K+1);
    J := LastDelimiter(CommandDelim, VarBody)+1;
    VarBody := copy(VarBody, J, length(VarBody));
    delete(Response, K+J-1, length(VarBody));
    insert(VarBody, Response, pos(DeclareJS + JSName + '=new', Response));
  end;
end;

{
Concats two JS commands only to translate nested Object Pascal typecasts as:
@param PrevCommand Command already present in Response that will be concatenated with NextCommand
@param NextCommand Command that will be concatenated with PrevCommand.
@return The JS commands concatenated
@example <code>
TExtGridRowSelectionModel(GetSelectionModel).SelectFirstRow;
// It's usually could be translated to:
O1.getSelectionModel;
O1.selectFirstRow;
// instead of:
O1.getSelectionModel.selectFirstRow;</code>
}
function TExtThread.JSConcat(PrevCommand, NextCommand : string) : string;
var
  I , J : integer;
begin
  J := ExtPascalUtils.RPosEx(PrevCommand, Response, 1);
  I := Pos('.', NextCommand);
  if (I <> 0) and (J <> 0) then begin
    NextCommand := copy(NextCommand, I, length(NextCommand));
    Result := copy(PrevCommand, 1, length(PrevCommand)-1) + NextCommand;
    delete(Response, J + length(PrevCommand)-1, 1);
    insert(NextCommand, Response, J + length(PrevCommand))
  end
  else
    Result := PrevCommand;
end;

procedure TExtThread.JSSleep(MiliSeconds : integer); begin
  JSCode('sleep(' + IntToStr(MiliSeconds) + ');')
end;

procedure TExtObject.JSSleep(MiliSeconds : integer); begin
  JSCode('sleep(' + IntToStr(MiliSeconds) + ');')
end;

function TExtObject.MethodURI(Method : TExtProcedure; Params : array of const) : string; begin
  Result := MethodURI(Method);
  if length(Params) <> 0 then begin
    if pos('?', Result) = 0 then Result := Result + '?';
    Result := Result + FormatParams('TExtObject.MethodURI', Params)
  end;
end;

function TExtObject.MethodURI(Method : TExtProcedure) : string;
var
  MetName, ObjName : string;
begin
  FindMethod(Method, MetName, ObjName);
  Result := CurrentWebSession.MethodURI(MetName) + IfThen(ObjName = '', '', '?Obj=' + ObjName);
end;

function TExtObject.MethodURI(MethodName : string; Params : array of const) : string; begin
  Result := CurrentWebSession.MethodURI(MethodName) + IfThen(length(Params) = 0, '', '?' + FormatParams(MethodName, Params))
end;

function TExtObject.MethodURI(MethodName : string): string; begin
  Result := CurrentWebSession.MethodURI(MethodName)
end;

{
Does tasks related to the Request that occur before the method call invoked by Browser (PATH-INFO)
1. Detects the browser language.
2. If that language has corresponding JS resource file in framework uses it, for example: '/ext/source/locale/ext-lang-?????.js',
3. Else uses the default language (English).
4. Identify the browser.
5. Tests if is an AJAX request.
6. Tests if cookies are enabled.
@return False if Cookies are disable or if is Ajax executing the first thread request else returns true.
}
function TExtThread.BeforeHandleRequest : boolean;
var
  I : integer;
begin
  Result := true;
  if FLanguage = '' then begin // Set language
    FLanguage := RequestHeader['HTTP_ACCEPT_LANGUAGE'];
    I := pos('-', FLanguage);
    if I <> 0 then begin
      FLanguage := copy(FLanguage, I-2, 2) + '_' + Uppercase(copy(FLanguage, I+1, 2));
      if not FileExists(RequestHeader['DOCUMENT_ROOT'] + ExtPath + SourcePath + '/locale/ext-lang-' + FLanguage + '.js') then
        FLanguage := copy(FLanguage, 1, 2)
    end;
  end;
  IsAjax := (RequestHeader['HTTP_X_REQUESTED_WITH'] = 'XMLHttpRequest') or IsUpload;
  if IsAjax then begin
    if Cookie['FCGIThread'] = '' then begin
      ErrorMessage('This web application requires Cookies enabled to AJAX works.');
      Result := false;
    end
    else
      if NewThread or RequiresReload then begin
        ErrorMessage('Session expired or lost.<br/>A new session will be created now.', 'window.location.reload()');
        RequiresReload := true;
        Result := false;
      end
  end
  else
    RequiresReload := false;
  JSReturns := TStringList.Create;
end;

// Override this method to change ExtPath, ImagePath, ExtBuild and Charset default values
procedure TExtThread.InitDefaultValues; begin
  inherited;
{$IFDEF CacheFly}
  ExtPath       := 'http://extjs.cachefly.net/ext-3.2.1';
{$ELSE}
  ExtPath       := '/ext';
{$ENDIF}
  ImagePath     := '/images';
  ExtBuild      := 'ext-all';
  Charset       := 'utf-8'; // 'iso-8859-1'
  UpLoadPath    := '/uploads';
end;

{
Creates a session to handle a new requests
@param AOwner You do not need to know this :)
}
constructor TExtThread.Create(AOwner: TObject); begin
  inherited;
end;

// config will be read once, only on new client thread construction
procedure TExtThread.AfterNewSession; begin
  inherited;
  {$IFDEF HAS_CONFIG}
  ReadConfig;
  {$ENDIF}
end;

{$IFDEF HAS_CONFIG}
// Read ExtPath, ImagePath and Theme from configuration file
procedure TExtThread.ReadConfig; begin
  with Application do
    if HasConfig then begin
      ExtPath := Config.ReadString('FCGI', 'ExtPath', ExtPath);
      if pos('/', ExtPath) <> 1 then ExtPath := '/' + ExtPath;
      ImagePath := Config.ReadString('FCGI', 'ImagePath', ImagePath);
      if pos('/', ImagePath) <> 1 then ImagePath := '/' + ImagePath;
      Theme   := Config.ReadString('FCGI', 'ExtTheme', Theme);
      Charset := Config.ReadString('FCGI', 'Charset', Charset);
    end;
end;

// Re-read config file if password is right
procedure TExtThread.DoReconfig;
{$IFDEF DEBUGJS}
var
  I : integer;
{$ENDIF}
begin
  ReadConfig;
  {$IFDEF DEBUGJS}
  // show applied configuration values (only during debugging)
  with TStringList.Create do begin
    LoadFromFile(Config.FileName);
    Response := 'RECONFIG: Application is reconfigured with the following values:<p>';
    for I := 0 to Count-1 do
      Response := Response + Strings[I] + '<br>';
    SendResponse(Response);
    Free;
  end;
  {$ENDIF}
end;
{$ENDIF}

// Calls events using Delphi style
procedure TExtThread.HandleEvent;
var
  Obj : TExtObject;
begin
  if Query['IsEvent'] = '1' then begin
    Obj := TExtObject(GarbageFind(Query['Obj']));
    if not Assigned(Obj) then
      OnError('Object not found in session list. It could be timed out, refresh page and try again', 'HandleEvent', '')
    else
      Obj.HandleEvent(Query['Evt']);
  end;
end;

{
Does tasks after Request processing.
1. Extracts Comments, auxiliary delimiters, and sets:
2. HTML body,
3. Title,
4. Application icon,
5. Charset,
6. ExtJS CSS,
7. ExtJS libraries,
8. ExtJS theme,
9. ExtJS language,
10. Additional user styles,
11. Additional user libraries,
12. Additional user JavaScript,
13. ExtJS invoke,
14. Handlers for AJAX response and
15. If DEBUGJS conditional-define is active:
  15.1. Uses CodePress library with syntax highlight,
  15.2. Generates JS code to enhance AJAX debugging, using Firefox, and
  15.3. Formats AJAX response code for easy debugging.
}
procedure TExtThread.AfterHandleRequest;

  procedure HandleJSReturns;
  var
    I : integer;
  begin
    if (JSReturns <> nil) and (JSReturns.Count <> 0) then
      for I := 0 to JSReturns.Count-1 do begin
        Response := AnsiReplaceStr(Response, JSReturns.Names[I], JSReturns.ValueFromIndex[I]);
        Response := AnsiReplaceStr(Response, IntToStr(StrToInt(JSReturns.Names[I])), JSReturns.ValueFromIndex[I]);
      end;
    FreeAndNil(JSReturns);
  end;

var
  I, J : integer;
begin
  if IsDownLoad or IsUpLoad then exit;
  I := pos('/*', Response);
  while I <> 0 do begin // Extracts comments
    J := PosEx('*/', Response, I);
    delete(Response, I, J - I + 2);
    I := PosEx('/*', Response, I);
  end;
  HandleJSReturns;
  Response := AnsiReplaceStr(AnsiReplaceStr(Response, CommandDelim, ''), IdentDelim, ''); // Extracts aux delimiters
  if not IsAjax then begin
    ContentType := 'text/html; charset=' + Charset;
    Response := IfThen(HTMLQuirksMode, '<!docttype html public><html>',
      '<?xml version=1.0?><!doctype html public "-//W3C//DTD XHTML 1.0 Strict//EN">'^M^J'<html xmlns=http://www.w3org/1999/xthml>') + ^M^J +
      '<head>'^M^J +
      '<title>' + Application.Title + '</title>'^M^J +
      IfThen(Application.Icon = '', '', '<link rel="shortcut icon" href="' + ImagePath + '/' + Application.Icon + '"/>'^M^J) +
      '<meta http-equiv="content-type" content="charset=' + Charset + '" />'^M^J +
      '<link rel=stylesheet href="' + ExtPath + '/resources/css/' + ExtBuild + '.css" />'^M^J +
      '<script src="' + ExtPath + '/adapter/ext/ext-base.js"></script>'^M^J +
      '<script src="' + ExtPath + '/' + ExtBuild + {$IFDEF DebugExtJS}'-debug'+{$ENDIF} '.js"></script>'^M^J +
      {$IFDEF DEBUGJS}'<script src="/codepress/Ext.ux.CodePress.js"></script>'^M^J +{$ENDIF}
      IfThen(Theme = '', '', '<link rel=stylesheet href="' + ExtPath + '/resources/css/xtheme-' + Theme + '.css" />'^M^J) +
      IfThen(FLanguage = 'en', '', '<script src="' + ExtPath + SourcePath + '/locale/ext-lang-' + FLanguage + '.js"></script>'^M^J) +
      GetStyle + Libraries +
      '</head>'^M^J +
      '<body><div id=body>'^M^J +
      '<div id=loading style="position:absolute;font-family:verdana;top:40%;left:40%">'^M^J +
      '<img src="' + ExtPath + '/resources/images/default/shared/loading-balls.gif"/>' +
      ' Loading ' + Application.Title + '...</div>'^M^J +
      '</div><noscript>This web application requires JavaScript enabled</noscript></body>'^M^J +
      '<script>'^M^J +
      {$IFDEF DEBUGJS}BeautifyJS{$ENDIF}
      (IfThen(CustomJS = '', '', CustomJS + ^M^J) +
      'function AjaxError(m){Ext.Msg.show({title:"Ajax Error",msg:m,icon:Ext.Msg.ERROR,buttons:Ext.Msg.OK});};' +
      {$IFDEF DEBUGJS}
      'function AjaxSource(t,l,s){var w=new Ext.Window({title:"Ajax error: "+t+", Line: "+' + IfThen(Browser=brFirefox, '(l-%%)', '"Use Firefox to debug"') +
      ',width:600,height:400,modal:true,items:[new Ext.ux.CodePress({language:"javascript",readOnly:true,code:s})]});w.show();' +
      'w.on("resize",function(){w.items.get(0).resize();});};' +
      'function AjaxSuccess(response){try{eval(response.responseText);}catch(err){AjaxSource(err.message,err.lineNumber,response.responseText);}};' +
      {$ELSE}
      'function AjaxSuccess(response){try{eval(response.responseText);}catch(err){AjaxError(err.message+"<br/>Use DebugJS define to enhance debugging<br/>"+response.responseText);}};' +
      {$ENDIF}
      'function sleep(ms){var start=new Date().getTime();for(var i=0;i<1e7;i++)if((new Date().getTime()-start)>ms)break;};'+
      'function AjaxFailure(){AjaxError("Server unavailable, try later.");};' +
      'Ext.onReady(function(){' +
      'Ext.get("loading").remove();' +
      'Ext.BLANK_IMAGE_URL="' + ExtPath + '/resources/images/default/s.gif";TextMetrics=Ext.util.TextMetrics.createInstance("body");'+
      'Download=Ext.DomHelper.append(document.body,{tag:"iframe",cls:"x-hidden"});' +
      Response) + '});'^M^J +
      '</script>'^M^J^M^J'</html>';
    {$IFDEF DEBUGJS}
    Response := AnsiReplaceStr(Response, '%%', IntToStr(CountStr(^M^J, Response, 'eval('))); // eval() line number
    {$ENDIF}
  end
  else begin
    ContentType := 'text/javascript; charset=' + Charset;
    {$IFDEF DEBUGJS}
    Response := BeautifyJS(Response);
    {$ENDIF}
  end;
end;

{
Returns a unique numeric sequence to identify a JS object, list or attribute in this session.
This sequence will be used by Self-translating process imitating a Symbol table entrance.
}
function TExtThread.GetSequence : string; begin
  Result := IntToHex(Sequence, 1);
  inc(Sequence);
end;

{ ExtObjectList }

{
Creates a TExtObjectList instance.
@param pOwner TExtObject that owns this list
@param pAttribute JS attribute name in TExtObject to this list
}
constructor TExtObjectList.Create(pOwner : TExtObject = nil; pAttribute : string = ''); begin
  Attribute := pAttribute;
  Owner     := pOwner;
  Created   := true;
  if CurrentWebSession <> nil then
    JSName := 'O' + IdentDelim + TExtThread(CurrentWebSession).GetSequence + IdentDelim;
end;

{
Creates a singleton TExtObjectList instance, used usually by Parser only.
@param pAttribute JS attribute name in TExtObject to this list in the form 'Owner.attribute'
}
constructor TExtObjectList.CreateSingleton(pAttribute : string); begin
  Attribute := pAttribute;
end;

// Frees this list and all objects linked in it
destructor TExtObjectList.Destroy; begin
  SetLength(FObjects, 0);
  inherited;
end;

{
Adds a <link TExtObject> in this list and generates the corresponding JS code in the Response.
@param Obj <link TExtObject> to add in the list
}
procedure TExtObjectList.Add(Obj : TExtObject);
var
  ListAdd, Response, OwnerName : string;
begin
  if length(FObjects) = 0 then
    if Owner <> nil then begin
      if pos('/*' + Owner.JSName + '*/', CurrentWebSession.Response) <> 0 then
        Owner.JSCode(Attribute + ':[/*' + JSName + '*/]', Owner.JSName)
    end
    else
      with TExtThread(CurrentWebSession) do begin
        JSCode(DeclareJS + JSName + '=[/*' + JSName + '*/];');
        GarbageAdd(JSName, nil);
      end;
  SetLength(FObjects, length(FObjects) + 1);
  FObjects[high(FObjects)] := Obj;
  Response := CurrentWebSession.Response;
  if Owner <> nil then
    OwnerName := Owner.JSName
  else
    OwnerName := '';
  if not Obj.Created or (pos(JSName, Response) = 0) then begin
    if TExtThread(CurrentWebSession).IsAjax and (OwnerName <> '') then
      if not Obj.Created then
        if pos(JSName, Response) = 0 then begin
          ListAdd := DeclareJS + Obj.JSName + '=' + OwnerName + '.add(%s);';
          TExtThread(CurrentWebSession).GarbageAdd(Obj.JSName, nil);
        end
        else
          ListAdd := '%s'
      else
        ListAdd := OwnerName + '.add(' + Obj.JSName + ');'
    else
      ListAdd := '%s';
    if Attribute = 'items' then // Generalize it more if necessary
      ListAdd := Format(ListAdd, ['new ' + Obj.JSClassName + '({/*' + Obj.JSName + '*/})'])
    else
      ListAdd := Format(ListAdd, ['{/*' + Obj.JSName + '*/}']);
  end
  else
    if pos(Obj.JSName + '.clone', Obj.JSCommand) = 1 then
      ListAdd := Obj.ExtractJSCommand
    else
      ListAdd := Obj.JSName;
  Obj.Created := true;
  Obj.JSCode(ListAdd, JSName, OwnerName);
  if Obj.JSClassName = 'Ext.ux.CodePress' then
    Owner.JSCode(OwnerName + '.on("resize", function(){' + OwnerName + '.items.get(' + IntToStr(high(FObjects)) + ').resize();});');
end;

{
Returns the Ith object in the list, starts with 0.
@param I Position in list
@return <link TExtObject>
}
function TExtObjectList.GetFObjects(I : integer) : TExtObject; begin
  if (I >= 0) and (I <= high(FObjects)) then
    Result := FObjects[I]
  else
    Result := nil
end;

function TExtObjectList.IndexOf(Obj: TExtObject): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := Low(FObjects) to High(FObjects) do
  begin
    if FObjects[I] = Obj then
    begin
      Result := I;
      Break;
    end;
  end;
end;

procedure TExtObjectList.Remove(Obj: TExtObject);
var
  LIndex: Integer;
  LLength: Integer;
  I: Integer;
begin
  LIndex := IndexOf(Obj);
  if LIndex >= 0 then
  begin
    LLength := Length(FObjects);
    for I := LIndex + 1 to LLength - 1 do
      FObjects[I - 1] := FObjects[I];
    SetLength(FObjects, LLength - 1);
  end;
end;

// Returns the number of Objects in the list
function TExtObjectList.Count : integer; begin
  Result := length(FObjects)
end;

{ ExtObject }

// Set an unique <link TExtObject.JSName, JSName> using <link TExtThread.GetSequence, GetSequence>
procedure TExtObject.CreateJSName; begin
  FJSName := 'O' + IdentDelim + TExtThread(CurrentWebSession).GetSequence + IdentDelim;
end;

{
Creates a singleton TExtObject instance, used usually by Parser only.
@param Attribute JS attribute name in TExtObject to this list in the form 'Owner.attribute'
}
constructor TExtObject.CreateSingleton(Attribute : string = ''); begin
  if Attribute = '' then
    FJSName := JSClassName
  else
    FJSName := Attribute;
  InitDefaults;
end;

{
Converts a TExtFormField length in characters to pixels to use in Width property.
Uses dynamic JS in browser.
@param Chars Field length in characters
@return Pixels used by browser to render these Chars
}
function TExtObject.CharsToPixels(Chars : integer) : integer; begin
  // + 16 sort of compensates for text-to-border left and right margins.
  Result := JSExpression('(%s * %d) + 16', [ExtUtilTextMetrics.GetWidth('g'), Chars]);
end;

{
Converts a TExtFormTextArea height in characters to pixels to use in Height property.
Uses dynamic JS in browser.
@param Lines TextArea height in characters.
@return Pixels used by browser to render these Lines
}
function TExtObject.LinesToPixels(Lines : integer) : integer; begin
  Result := JSExpression('%s * %.2f', [ExtUtilTextMetrics.GetHeight('W'), Lines * 0.8]);
end;

{
When assign value to a config property, check if it is in creating process.
If not then config property code will redirect to a relationed method if it exists.
@param JSName Objects name to be searched in generated script
@return true if the JSName object has a configuration in this request, false if not
@example <code>
//doesn't matter if you are into Create block or assign to previous ajax created object
//O1.title will be mapped to O1.setTitle('new title', ''); by ExtToPascal wrapper
O1.title = 'new title';
</code>
}
function TExtObject.ConfigAvailable(JSName : string) : boolean; begin
  Result := pos('/*' + JSName + '*/', TExtThread(CurrentWebSession).Response) <> 0;
end;

{
Used by <link TExtObject.Create, Create> to initialize the JSName, to <link TFCGIThread.AddToGarbage, add to Garbage Collector>
and to generate <link TExtObject.JSCode, JS code>
@param JS JS constructor for the JS <color red>new</color> command
@see CreateVarAlt
}
procedure TExtObject.CreateVar(JS : string); begin
  CreateJSName;
  TExtThread(CurrentWebSession).GarbageAdd(JSName, Self);
  insert('/*' + JSName + '*/', JS, length(JS)-IfThen(pos('});', JS) <> 0, 2, 1));
  Created := true;
  JSCode(CommandDelim + DeclareJS + JSName + IfThen(JS[1] = '(', '= ', '=new ') + JS);
  JSCode(JSName + '.nm="' + JSName + '";');
end;

{
Alternate create constructor, it is an ExtJS fault
@see CreateVar
}
procedure TExtObject.CreateVarAlt(JS : string); begin
  CreateJSName;
  TExtThread(CurrentWebSession).GarbageAdd(JSName, Self);
  insert('/*' + JSName + '*/', JS, length(JS)-IfThen(pos('});', JS) <> 0, 2, 1));
  Created := true;
  JSCode(CommandDelim + DeclareJS + JSName + '= ' + JS);
  JSCode(JSName + '.nm="' + JSName + '";');
end;

// Deletes JS object from Browser memory
procedure TExtObject.Delete; begin
  JSCode('delete ' + JSName + ';')
end;

// <link TFCGIThread.DeleteFromGarbage, Removes object from Garbage Collector> if is not in a Garbage Collector call
procedure TExtObject.DeleteFromGarbage; begin
  if CurrentWebSession <> nil then TExtThread(CurrentWebSession).GarbageDelete(Self);
end;

// Calls Ext JS <b>destroy()</b> method if it exists else calls the JS <b>delete</b> command
function TExtObject.DestroyJS : TExtFunction; begin
  Delete;
  Result := TExtFunction(Self)
end;

function TExtObject.GetDownloadJS(Method: TExtProcedure; Params: array of const): string;
var
  P, MetName, ObjName : string;
begin
  FindMethod(Method, MetName, ObjName);
  P := FormatParams(MetName, Params);
  if ObjName <> '' then begin
    if P <> '' then P := P + '&';
    P := P + 'Obj=' + ObjName;
  end;
  if P <> '' then P := '?' + P;
  Result := 'Download.src="' + CurrentWebSession.MethodURI(MetName) + P + '";';
end;

procedure TExtObject.Download(Method: TExtProcedure; Params: array of const);
begin
  JSCode(GetDownloadJS(Method, Params));
end;

procedure TExtObject.Download(Method: TExtProcedure);
begin
  Download(Method, []);
end;

// <link TExtObject.DeleteFromGarbage, Removes object from Garbage Collector> and frees it
destructor TExtObject.Destroy; begin
  if CurrentWebSession <> nil then TExtThread(CurrentWebSession).GarbageRemove(Self);
  inherited;
end;

{
Creates a TExtObject and generate corresponding JS code using <link TExtObject.JSCode, Self-translating>
@param Owner Optional parameter used internally by <link TExtObject.JSObject, JSObject> and <link TExtObject.JSArray, JSArray> only
}
constructor TExtObject.Create(Owner : TExtObject = nil); begin
  //if Owner = nil then
  CreateVar(JSClassName + '({});');// else Created := True;
end;

{
Used by Parser to build <link TExtObject.InitDefaults, InitDefaults> methods used to initialize public JS properties in a TExtObject
@param Owner TExtObject where this property is declared
@param Attribute Public JS property name
}
constructor TExtObject.CreateInternal(Owner : TExtObject; Attribute : string); begin
  FJSName := Owner.JSName + '.' + Attribute;
  Created := true;
end;

// Returns 'Object' that is the default class name for Ext JS objects
function TExtObject.JSClassName : string; begin
  Result := 'Object'
end;

{
Tests if a class name is parent of this object
@param CName Class name with "T" prefix
@return True if CName is parent of this object and false if not
}
function TExtObject.IsParent(CName : string) : boolean;
var
  Cls : TClass;
begin
  if (CName <> '') and (CName[1] = 'T') then begin
    Result := true;
    Cls    := ClassType;
    while Cls.ClassName <> 'TExtFunction' do begin
      if Cls.ClassName = CName then exit;
      Cls := Cls.ClassParent
    end;
  end;
  Result := false;
end;

// Get last JSCommand emited by this object
function TExtObject.GetJSCommand : string;
var
  I : integer;
begin
  I := LastDelimiter(JSDelim, FJSCommand);
  if I = 0 then
    Result := FJSCommand
  else
    Result := copy(FJSCommand, I+1, length(FJSCommand));
end;

{
Pushes a JSCommand to this object
@param Value JSCommand
}
procedure TExtObject.SetJSCommand(const Value : string);
var
  I, J : integer;
begin
  if Value = '' then begin
    if FJSCommand <> '' then begin
      I := LastDelimiter(JSDelim, FJSCommand);
      J := pos(IdentDelim + '.get', FJSCommand);
      if (I = 0) or (J = 0) or (J > I) then
        FJSCommand := ''
      else
        System.Delete(FJSCommand, I, length(FJSCommand));
    end
  end
  else
    if pos(IdentDelim + '.', Value) = 0 then
      FJSCommand := Value
    else
      FJSCommand := FJSCommand + IfThen(FJSCommand = '', '', JSDelim) + Value
end;

// Pops first JSCommand emited by this object
function TExtObject.PopJSCommand : string;
var
  I : integer;
begin
  I := FirstDelimiter(JSDelim, FJSCommand);
  if I = 0 then begin
    Result := FJSCommand;
    FJSCommand := '';
  end
  else begin
    Result := copy(FJSCommand, 1, I-1);
    System.Delete(FJSCommand, 1, I);
  end;
end;

function TExtObject.RequestDownload(Method : TExtProcedure; Params : array of const): TExtFunction;
begin
  Result := JSFunction(GetDownloadJS(Method, Params));
end;

function TExtObject.RequestDownload(Method : TExtProcedure) : TExtFunction; begin
  Result := RequestDownload(Method, [])
end;

{
Starts Self-translating mechanism invoking <link TExtThread.JSCode, JSCode>.
Invokes <link TExtThread.JSConcat, JSConcat> if identify a nested typecast
@param JS JS commands or declarations
@param pJSName Optional, by default is the <link TExtObject.JSName, JSName>, when used by <link TExtObjectList.Add> is the TExtObjectList.JSName
@param pOwner Optional, used by <link TExtObjectList.Add> only to pass the TExtObject owner list
}
procedure TExtObject.JSCode(JS : string; pJSName : string = ''; pOwner : string = '');
var
  lJSName, JSC : string;
begin
  if JS <> '' then begin
    if (pos('.nm="', JS) = 0) and (JS[length(JS)] = ';') and not(pos(DeclareJS, JS) in [1, 2]) then begin
      if (JSCommand <> '') and (pJSName <> '') and not IsParent(pJSName) then begin
        JSC := JSCommand;
        JSCommand := '';
        JSCommand := TExtThread(CurrentWebSession).JSConcat(JSC, JS);
        exit;
      end;
      if not(pos(IdentDelim + '.get', FJSCommand) in [4..9]) then FJSCommand := '';
      JSCommand := JS;
    end;
    // else JSCommand := '';
    if (pJSName = '') or (pos('T', pJSName) = 1) then
      lJSName := JSName
    else
      lJSName := pJSName;
    TExtThread(CurrentWebSession).JSCode(JS, pJSName, lJSName, pOwner);
  end;
end;

{
Adds this object in a list.
If called as constructor creates the object before adds it to the list.
@param List An instanciated <link TExtObjectList>
}
constructor TExtObject.AddTo(List : TExtObjectList); begin
  if JSName = '' then begin
    CreateVar(JSClassName + '({});');
    InitDefaults;
  end;
  List.Add(Self);
end;

// Inits a JS Object with a <link TExtFunction>
constructor TExtObject.Init(Method : TExtFunction); begin
  CreateJSName;
  TExtThread(CurrentWebSession).GarbageAdd(JSName, nil);
  Created := true;
  JSCode(CommandDelim + DeclareJS + JSName + '=' + Method.ExtractJSCommand + ';');
end;

// Inits a JS Object with a JS command
constructor TExtObject.Init(Command : string); begin
  CreateJSName;
  TExtThread(CurrentWebSession).GarbageAdd(JSName, Self);
  Created := true;
  JSCode(CommandDelim + DeclareJS + JSName + '=' + Command + ';');
end;

procedure TExtObject.InitDefaults; begin end;

{
Generates JS code to declare an inline JS Array.
@param JSON JavaScript Object Notation, the body of Array declaration
@param SquareBracket If true surrounds the array with []. Default is true.
@return <link TExtObjectList> to be used in assigns
}
function TExtObject.JSArray(JSON : string; SquareBracket : boolean = true) : TExtObjectList; begin
  Result := TExtObjectList(TExtObject.Create(Self));
  If SquareBracket then // FPC dont support IfThen with ansistrings by default
    TExtObject(Result).FJSName := '[' + JSON + ']'
  else
    TExtObject(Result).FJSName := JSON;
end;

(*
Generates JS code to declare an inline generic JS object.
It is necessary in 3 cases:
1. When the Ext JS documentation informs that the attribute is an object without any particular type (Object),
   as JavaScript language is almost typeless it happens eventually. That would be equivalent to the type Variant of VB or Delphi.
   Examples include data records.
2. There are omissions in the documentation and attribute actually belongs to a specific class, in this case use the JSObject method,
   do a typecast or declare in ExtFixes.txt file, this allows to register in the Wrapper the omissions of the
   documentation or the framework.
3. There are omissions in the framework, ie should be a specific class. Read its attributes in description contained
   in the documentation and declare them in ExtFixes.txt for the Wrapper to recognize them or use JSObject method.
@param JSON JavaScript Object Notation, the body of JS object declaration
@param ObjectConstructor Instantiate this object with a specific constructor. Default is ''
@param CurlyBracket If true surrounds the JSON with {}. Default is true.
@return <link TExtObject> to be used in assigns
*)
function TExtObject.JSObject(JSON : string; ObjectConstructor : string = ''; CurlyBracket : boolean = true) : TExtObject; begin
  Result := TExtObject.Create(Self);
  if CurlyBracket then // FPC dont support IfThen with ansistrings by default
    Result.FJSName := '{' + JSON + '}'
  else
    Result.FJSName := JSON;
  if ObjectConstructor <> '' then
    Result.FJSName := 'new ' + ObjectConstructor + '(' + Result.FJSName + ')'
end;

function TExtObject.AddJSReturn(Expression : string; MethodsValues : array of const) : string;
var
  Command : string;
  I : integer;
begin
  with TExtThread(CurrentWebSession) do begin
    Result := '-$7' + GetSequence + '7';
    for I := 0 to high(MethodsValues) do
      with MethodsValues[I] do
        if VType = vtObject then begin
          Command := TExtFunction(VObject).ExtractJSCommand; // FPC idiosincrasy
          {$IFNDEF UNICODE}
          VAnsiString := pointer(Command);
          VType       := vtAnsiString;
          {$ELSE}
          VUnicodeString := pointer(Command);
          VType          := vtUnicodeString;
          {$ENDIF}
        end;
    JSReturns.Values[Result] := Format(Expression, MethodsValues, _JSFormatSettings);
  end;
end;

{
Lets use a JS expression to set a ExtJS property or parameter on browser side. <link TExtFunction, ExtJS function> in ExtJS properties and parameters.
The expression will be called in the browser side and should returns an integer.
@param Expression JS Expression using or not Delphi Format specifiers: (%s, %d and etc), use %s for ExtPascal methods' results and %d for integers
@param MethodsValues Array of Methods or Integer values to use in Expression
@return Integer Internal value to return used by ExtPascal to generate the corresponding Javascript code
@example <code>
Grid := TExtGridEditorGridPanel.Create;
with Grid do begin
  Width  := JSExpression(Panel.GetInnerWidth);
  Height := JSExpression(Panel.GetInnerHeight);
end;
FormWidth := 40;
with TExtWindow.Create do
  Width := JSExpression('%s * %d', [ExtUtilTextMetrics.GetWidth('g'), FormWidth]);
  // or Width := JSExpression('Ext.util.TextMetrics.getWidth("g") * ' + IntToStr(FormWidth), []);
</code>
}
function TExtObject.JSExpression(Expression : string; MethodsValues : array of const) : integer; begin
  Result := StrToInt(AddJSReturn(Expression, MethodsValues))
end;

// Shortcut version of JSExpression method
function TExtObject.JSExpression(Method : TExtFunction) : integer; begin
  Result := JSExpression('%s', [Method]);
end;

function TExtObject.JSString(Expression : string; MethodsValues : array of const) : string; begin
  Result := AddJSReturn('"+' + Expression + '+"', MethodsValues);
end;

function TExtObject.JSString(Method : TExtFunction) : string; begin
  Result := JSString('%s', [Method]);
end;

// Returns the JS command generated by a method
function TExtObject.JSMethod(Method : TExtFunction) : string; begin
  Result := Method.ExtractJSCommand;
end;

// Used internaly by the code generated by ExtToPascal. The generated code must overrides this method on extended classes where events can be handled
procedure TExtObject.HandleEvent(const AEvtName: string); begin end;

{
Generates JS code to declare an anonymous JS function with parameters
@param Params JS Parameters separated by commas
@param Body JS commands for JS function
@return <link TExtFunction> to use in event handlers
}
function TExtObject.JSFunction(Params, Body : string) : TExtFunction; begin
  Result := TExtFunction.Create(Self);
  Result.FJSName := 'function(' + Params + '){' + Body + '}';
end;

{
Generates JS code to declare an anonymous JS function without parameters
@param Body JS commands for JS function
@return <link TExtFunction> to use in event handlers
}
function TExtObject.JSFunction(Body : string) : TExtFunction; begin
  Result := JSFunction('', Body);
end;

{
Declares a named JS function with parameters.
@param Name JS function name
@param Params JS Parameters separated by commas
@param Body JS commands for JS function
}
procedure TExtObject.JSFunction(Name, Params, Body : string); begin
  JSCode('function ' + Name + '(' + Params + '){' + Body + '};');
end;

{
Generates an anonymous JS function from an Object Pascal procedure to use in event handlers.
To get event parameters use %0, %1 until %9 place holders.
@param Method Object Pascal procedure declared on the <link TExtThread> or in a <link TExtObject>
@param Silent Discards JS exceptions and returns immediately
@return <link TExtFunction> to use in event handlers
@example <code>
procedure TSamples.ReadButtonJS; begin
  ExtMessageBox.Alert('Browser Side: Button clicked', 'You clicked the "%0" button')
end;

procedure TSamples.MessageBoxes; begin
  with TExtPanel.Create do begin
    Title    := 'Message Boxes';
    Width    := 700;
    RenderTo := 'body';
    Frame    := true;
    with TExtButton.AddTo(Buttons) do begin
      Text    := 'Confirm Message';
      Handler := ExtMessageBox.Confirm('Confirm', 'Are you sure?', JSFunction(ReadButtonJS));
    end;
  end;
</code>
@example
<code>
procedure TSamples.SelectNodeEventBrowserSide; begin
  ExtMessageBox.Alert('Browser Side', '%0.text')
end;

procedure TSamples.BorderLayout;
var
 Tree : TExtTreeTreePanel;
 Root, Node : TExtTreeTreeNode;
begin
  Tree := TExtTreeTreePanel.Create;
  Tree.Border := false;
  //set root node
  Root := TExtTreeTreeNode.Create;
  with Root do begin
    Text := 'Root';
    AllowChildren := True;
    Expandable := True;
    Expanded := True;
    Leaf := False;
    on('click', JSFunction(SelectNodeEventBrowserSide));
  end;
: : : : : : :</code>
}
function TExtObject.JSFunction(Method : TExtProcedure; Silent : boolean = false) : TExtFunction;
var
  CurrentResponse : string;
begin
  InJSFunction := true;
  Result := TExtFunction(Self);
  with TExtThread(CurrentWebSession) do begin
    CurrentResponse := Response;
    Response := '';
    Method;
    JSCommand := '';
    if Silent then
      JSCommand := 'try{' + Response + '}catch(e){};'
    else
      JSCommand := Response;
    Response  := CurrentResponse;
    JSCode(JSCommand);
  end;
  InJSFunction := false;
end;

{
Invokes an Object Pascal published procedure in AJAX mode.
To get event parameters use %0, %1 until %9 place holders.<p>
@param Method Published procedure to invoke
@return <link TExtFunction> to use in event handlers
@example <code>
procedure TSamples.AddTab; begin // published method
  inc(TabIndex);
  with TExtPanel.AddTo(Tabs.Items) do begin
    Title    := 'New Tab ' + IntToStr(TabIndex);
    IconCls  := 'tabs';
    Html     := 'Tab Body ' + IntToStr(TabIndex) + '<br/><br/>blahblah';
    Closable := true;
    if IsAjax then Show;
    Free;
  end;
end;

procedure TSamples.AdvancedTabs;
var
  I : integer;
begin
  with TExtButton.Create do begin
    RenderTo := 'body';
    Text     := 'Add Tab using AJAX!';
    IconCls  := 'new-tab';
    Handler  := Ajax(AddTab);
    Free;
  end;
  Tabs := TExtTabPanel.Create;
  with Tabs do begin
    RenderTo        := 'body';
    ActiveTabNumber := 0;
    ResizeTabs      := true; // turn on tab resizing
    MinTabWidth     := 115;
    TabWidth        := 135;
    Width           := 600;
    Height          := 150;
    Defaults        := JSObject('autoScroll:true');
    EnableTabScroll := true;
    Plugins         := JSObject('', 'Ext.ux.TabCloseMenu');
    for I := 1 to 7 do AddTab;
  end;
end;</code>
}
function TExtObject.Ajax(Method : TExtProcedure) : TExtFunction; begin
  Result := Ajax(Method, []);
end;

{
Invokes an Object Pascal published procedure with parameters in AJAX mode.
To get event parameters use %0 until %9 place holders.<p>
@param Method Published procedure to invoke
@param Params Array of Parameters, each parameter is a pair: Name, Value.
To get them on server side use <link TFCGIThread.Query> array property in AJAX method.
@return <link TExtFunction> to use in event handlers
@example <code>
procedure TSamples.CheckLogin; begin
  if true (*user account verification should be done here*) then
    with TExtWindow.Create do begin
      Title    := 'Login';
      Width    := 380;
      Height   := 140;
      Plain    := true;
      Layout   := 'fit';
      Closable := false;
      with TExtPanel.AddTo(Items) do begin
        Border    := false;
        BodyStyle := 'padding: 5px 8px';
        HTML      := 'Welcome, ' + Query['UserName'] + '.<br/>Password: ' + Query['Password'];
      end;
      Show;
    end
  else
    ExtMessageBox.Alert('Unknown', 'User is not known.');
end;

procedure TSamples.Login;
var
  UserName, Password : TExtFormTextField;
begin
  FormLogin := TExtWindow.Create;
  with FormLogin do begin
    Title    := 'Login';
    Width    := 380;
    Height   := 140;
    Plain    := true;
    Layout   := 'fit';
    Closable := false;
    with TExtFormFormPanel.AddTo(Items) do begin
      LabelWidth  := 70;
      Border      := false;
      XType       := 'form';
      ButtonAlign := 'right';
      BodyStyle   := 'padding: 10px 15px';
      DefaultType := 'textfield';
      Defaults    := JSObject('width: 250');
      UserName    := TExtFormTextField.Create;
      with UserName.AddTo(Items) do begin
        Name       := 'user';
        FieldLabel := 'Username';
        InputType  := 'textfield';
      end;
      Password := TExtFormTextField.Create;
      with Password.AddTo(Items) do begin
        Name       := 'pass';
        FieldLabel := 'Password';
        InputType  := 'password';
      end;
      with TExtButton.AddTo(Buttons) do begin
        Text    := 'LOGIN';
        Handler := Ajax(CheckLogin, ['UserName', UserName.GetValue, 'Password', Password.GetValue]);
      end;
    end;
    Show;
  end;
end;
</code>
}
function TExtObject.Ajax(Method : TExtProcedure; Params : array of const) : TExtFunction;
var
  MetName, ObjName : string;
begin
  Result := FindMethod(Method, MetName, ObjName);
  AjaxCode(MetName, IfThen(ObjName = '', '', 'Obj=' + ObjName), Params);
end;

{
Discovers the Pascal name and the JavaScript object name for a method
Raises an exception if method is not published
@return Self
}
function TExtObject.FindMethod(Method : TExtProcedure; var PascalName, ObjName : string) : TExtFunction;
var
  Obj : TObject;
begin
  InJSFunction := false;
  Obj := TMethod(Method).Data;
  PascalName := Obj.MethodName(@Method);
  if PascalName = '' then
    raise Exception.Create('Ajax: Method is not published')
  else begin
    if Obj is TExtObject then
      ObjName := TExtObject(Obj).JSName
    else
      ObjName := '';
    Result := TExtFunction(Self);
  end;
end;

// Ajax with raw string as params
function TExtObject.Ajax(Method : TExtProcedure; Params : string) : TExtFunction;
var
  MetName, ObjName : string;
begin
  Result := FindMethod(Method, MetName, ObjName);
  JSCode('Ext.Ajax.request({url:"' + MethodURI(MetName) + '",params:"Ajax=1&"+' +
    Params + IfThen(ObjName = '', '', '+"&Obj=' + ObjName + '"') + ',success:AjaxSuccess,failure:AjaxFailure});');
(*  JSCode('Ext.Ajax.request({url:"' + MethodURI(MetName) + '",params:{Ajax:1,' + Params +
    IfThen(ObjName = '', '', ',Obj:' + ObjName) + '},success:AjaxSuccess,failure:AjaxFailure});');*)
end;

// Ajax with JSFunction as params
function TExtObject.AjaxExtFunction(Method : TExtProcedure; Params : array of TExtFunction) : TExtFunction;
var
  I : integer;
  S : string;
begin
  S := '';
  for I := 0 to high(Params) do begin
    S := S + TExtObject(Params[I]).ExtractJSCommand;
    if I <> high(Params) then S := S + '+"&"+';
  end;
  Result := Ajax(Method, S);
end;

function TExtObject.AjaxForms(Method : TExtProcedure; Forms : array of TExtObject) : TExtFunction;
var
  I : integer;
  S : string;
begin
  S := '';
  for I := 0 to high(Forms) do begin
    if Forms[I] is TExtFormField then begin
      S := S + '"' + TExtFormField(Forms[I]).ID + '="+';
      TExtFormField(Forms[I]).GetValue;
    end
    else
      TExtFormBasicForm(TExtFormFormPanel(Forms[I]).GetForm).GetValues(true);
    S := S + TExtObject(Forms[I]).ExtractJSCommand;
    if I <> high(Forms) then S := S + '+"&"+';
  end;
  Result := Ajax(Method, S);
end;

function TExtObject.AjaxSelection(Method : TExtProcedure; SelectionModel : TExtObject; Attributes, TargetQueries : string; Params : array of const) : TExtFunction;
var
  CurrentResponse : string;
  MetName, ObjName : string;
  I : Integer;
  LParams : string;
  LAttributes : TStringList;
  LTargetQueries : TStringList;
  LBody : string;
begin
  LAttributes := TStringList.Create;
  try
    LAttributes.StrictDelimiter := True;
    LAttributes.Delimiter := ',';
    LAttributes.DelimitedText := Attributes;
    LTargetQueries := TStringList.Create;
    try
      LTargetQueries.StrictDelimiter := True;
      LTargetQueries.Delimiter := ',';
      LTargetQueries.DelimitedText := TargetQueries;

      Assert(LAttributes.Count > 0);
      Assert(LAttributes.Count = LTargetQueries.Count);

      InJSFunction := true;
      Result := TExtFunction(Self);
      FindMethod(Method, MetName, ObjName);
      with TExtThread(CurrentWebSession) do begin
        CurrentResponse := Response;
        Response := '';
        LParams := 'Obj=' + ObjName;
        for I := 0 to LAttributes.Count - 1 do begin
          JSCode(Format('var Sel%d=[];', [I]));
          LBody := LBody + Format('Sel%d.push(Rec.get("' + Trim(LAttributes[I]) + '"));', [I]);
          LParams := LParams + '&' + FormatParams(MetName, [Trim(LTargetQueries[I]), '%' + Format('Sel%d.toString()', [I])]);
        end;
        TExtGridRowSelectionModel(SelectionModel).Each(JSFunction('Rec', LBody));
        AjaxCode(MetName, LParams, Params);
        JSCommand := '';
        JSCommand := Response;
        Response  := CurrentResponse;
        JSCode(JSCommand);
      end;
      InJSFunction := false;
    finally
      FreeAndNil(LAttributes);
    end;
  finally
    FreeAndNil(LTargetQueries);
  end;
end;

function SurroundAjaxParam(Param : string) : string;
var
  I : integer;
begin
  I := pos('%', Param);
  if (I <> 0) and (I <> length(Param))  then
    if CharInSet(Param[I+1], ['0'..'9']) then
      Result := '"+' + Param + '+"'
    else
      Result := '"+' + copy(Param, I+1, length(Param)) + '+"'
  else
    Result := Param;
end;

function TExtObject.FormatParams(MethodName : string; Params : array of const) : string;
var
  I : integer;
begin
  Result := '';
  for I := 0 to high(Params) do
    with Params[I] do
      if Odd(I) then
        case VType of
          vtAnsiString : Result := Result + SurroundAjaxParam(string(VAnsiString));
          vtString     : Result := Result + SurroundAjaxParam(string(VString^));
          vtWideString : Result := Result + SurroundAjaxParam(string(VWideString));
          {$IFDEF UNICODE}
          vtUnicodeString : Result := Result + SurroundAjaxParam(string(VUnicodeString));
          {$ENDIF}
          vtObject     : Result := Result + '"+' + TExtObject(VObject).ExtractJSCommand + '+"';
          vtInteger    : Result := Result + IntToStr(VInteger);
          vtBoolean    : Result := Result + IfThen(VBoolean, 'true', 'false');
          vtExtended   : Result := Result + FloatToStr(VExtended^);
          vtCurrency   : Result := Result + CurrToStr(VCurrency^);
          vtInt64      : Result := Result + IntToStr(VInt64^);
          vtVariant    : Result := Result + string(VVariant^);
          vtChar       : Result := Result + string(VChar);
          vtWideChar   : Result := Result + VWideChar;
        end
      else begin
        if Result <> '' then Result := Result + '&';
        case VType of
          vtAnsiString : Result := Result + string(VAnsiString) + '=';
          vtString     : Result := Result + string(VString^) + '=';
          vtWideString : Result := Result + string(VWideString) + '=';
          {$IFDEF UNICODE}
          vtUnicodeString : Result := Result + string(VUnicodeString) + '=';
          {$ENDIF}
          vtChar       : Result := Result + string(VChar) + '=';
          vtWideChar   : Result := Result + VWideChar + '=';
        else
          JSCode('Ext.Msg.show({title:"Error",msg:"Ajax method: ' + MethodName +
            ' has an invalid parameter name in place #' + IntToStr(I+1) + '",icon:Ext.Msg.ERROR,buttons:Ext.Msg.OK});');
          exit;
        end;
      end;
end;

procedure TExtObject.AjaxCode(MethodName : string; RawParams : string; Params : array of const); begin
  JSCode('Ext.Ajax.request({url:"' + CurrentWebSession.MethodURI(MethodName) + '",params:"Ajax=1&' +
    IfThen(RawParams='', '', RawParams + '&') + FormatParams(MethodName, Params) +
    '",success:AjaxSuccess,failure:AjaxFailure});');
end;

// Internal Ajax generation handler treating IsEvent, when is true HandleEvent will be invoked instead published methods
function TExtObject.Ajax(MethodName : string; Params : array of const; IsEvent : boolean = false) : TExtFunction;
var
  lParams : string;
begin
  InJSFunction := false;
  Result  := TExtFunction(Self);
  lParams := IfThen(JSName = '', '', 'Obj=' + JSName);
  if IsEvent then begin
    lParams := lParams + '&IsEvent=1&Evt=' + MethodName;
    MethodName := 'HandleEvent';
  end;
  AjaxCode(MethodName, lParams, Params);
end;

{
Encapsulates JS commands in an anonymous JS function, find %0..%9 place holders and declares respective event parameters
@param Command JS command to convert to JS function
@return The code for an anonymous JS function with optional event parameters declared
}
function TExtObject.WriteFunction(Command : string) : string;
var
  I, J   : integer;
  Params : string;
begin
  Params := '';
  J := -1;
  I := pos('%', Command);
  while I <> 0 do begin
    if CharInSet(Command[I+1], ['0'..'9']) then begin
      Command[I] := 'P';
      J := max(J, StrToInt(Command[I+1]));
    end;
    I := posex('%', Command, I);
  end;
  for I := 0 to J do begin
    Params := Params + 'P' + IntToStr(I);
    if I <> J then Params := Params + ','
  end;
  I := LastDelimiter(';', copy(Command, 1, length(Command)-1));
  if (I = 0) or (I = length(Command)) and (pos('return ', Command) <> 1) then
    Command := 'return ' + Command
  else
    insert('return ', Command, I+1);
  Result := 'function(' + Params + '){' + Command + '}';
end;

{
Extracts <link TExtObject.JSCommand, JSCommand> from Response and resets JSCommand
@return JSCommand without the last char ';'
@see TExtThread.RemoveJS
}
function TExtObject.ExtractJSCommand : string; begin
  Result := PopJSCommand;
  TExtThread(CurrentWebSession).RemoveJS(Result);
  SetLength(Result, length(Result)-1);
end;

{
Frees TExtObject if object is not destroyed. Automatically calls Free for all components that it does reference.
Free is successful even if called repeatedly to the same object.
@param CallDestroyJS Calls <link TExtObject.DestroyJS, DestroyJS> if true to destroy or delete the JS object too
}
procedure TExtObject.Free(CallDestroyJS : boolean = false); begin
  if (Self <> nil) and Created then begin
    if CallDestroyJS then DestroyJS;
    Created := false;
    inherited Free;
  end;
end;

{
Converts an array of const to JSON (JavaScript Object Notation) to be used in constructors, JS Arrays or JS Objects
@param A Array of anytype variables
@return JSON representation of array
}
function TExtObject.VarToJSON(A : array of const) : string;
var
  I : integer;
  Command, JSName : string;
begin
  Result := '';
  I := 0;
  while I <= high(A) do begin
    with A[I] do
      case VType of
        vtObject: begin
          if VObject <> nil then begin
            Command := TExtObject(VObject).PopJSCommand;
            if (Command <> '') and A[I+1].VBoolean then begin
              Result := Result + WriteFunction(Command);
              TExtThread(CurrentWebSession).RemoveJS(Command);
            end
            else begin
              JSName := TExtObject(VObject).JSName;
              if InJSFunction and (pos(JSName, Command) = 1) then begin
                Result := Result + copy(Command, 1, length(Command)-1);
                TExtThread(CurrentWebSession).RemoveJS(Command);
              end
              else
                Result := Result + JSName;
            end;
          end
          else
            if Result = '' then
              Result := 'null'
            else begin
              inc(I, 2);
              continue;
            end;
          inc(I);
        end;
        vtAnsiString: Result := Result + StrToJS(string(VAnsiString));
        vtString:     Result := Result + StrToJS(string(VString^));
        vtWideString: Result := Result + StrToJS(string(VWideString));
        {$IFDEF UNICODE}
        vtUnicodeString: Result := Result + StrToJS(string(VUnicodeString));
        {$ENDIF}
        vtInteger:    Result := Result + IntToStr(VInteger);
        vtBoolean:    Result := Result + IfThen(VBoolean, 'true', 'false');
        vtExtended:   Result := Result + AnsiReplaceStr(FloatToStr(VExtended^), ',', '.');
        vtCurrency:   Result := Result + CurrToStr(VCurrency^);
        vtInt64:      Result := Result + IntToStr(VInt64^);
        vtVariant:    Result := Result + string(VVariant^);
        vtChar:       Result := Result + string(VChar);
        vtWideChar:   Result := Result + VWideChar;
      end;
    if I < high(A) then Result := Result + ',';
    inc(I);
  end;
  if (Result <> '') and (Result[length(Result)] = ',') then System.Delete(Result, length(Result), 1);
end;

{
Converts a <link TExtObjectList> to JSON (JavaScript Object Notation) to be used in constructors, JS Arrays or JS Objects
@param Exts An TExtObjectList to convert
@return JSON representation of Exts
}
function TExtObject.VarToJSON(Exts : TExtObjectList) : string; begin
  if Exts.ClassName = 'TExtObjectList' then
    Result := Exts.JSName
  else
    Result := TExtObject(Exts).JSName
end;

{
Converts an <link TArrayOfString, array of strings> to JSON (JavaScript Object Notation) to be used in constructors, JS Arrays or JS Objects
@param Strs An <link TArrayOfString, array of strings> to convert
@return JSON representation of Strs
}
function TExtObject.ArrayToJSON(Strs : {$IF Defined(FPC) or (RTLVersion < 20)}TArrayOfString{$ELSE}array of string{$IFEND}) : string;
var
  I : integer;
begin
  Result := '[';
  for I := 0 to high(Strs) do begin
    Result := Result + StrToJS(Strs[I]);
    if I < high(Strs) then Result := Result + ',';
  end;
  Result := Result + ']'
end;

{
Converts an <link TArrayOfInteger, array of integers> to JSON (JavaScript Object Notation) to be used in constructors, JS Arrays or JS Objects
@param Ints An <link TArrayOfInteger, array of integers> to convert
@return JSON representation of Ints
}
function TExtObject.ArrayToJSON(Ints : {$IF Defined(FPC) or (RTLVersion < 20)}TArrayOfInteger{$ELSE}array of integer{$IFEND}) : string;
var
  I : integer;
begin
  Result := '[';
  for I := 0 to high(Ints) do begin
    Result := Result + IntToStr(Ints[I]);
    if I < high(Ints) then Result := Result + ',';
  end;
  Result := Result + ']'
end;

// Aux function used internaly by ExtToPascal to override HandleEvent method
function TExtObject.ParamAsInteger(ParamName : string) : integer; begin
  Result := StrToIntDef(CurrentWebSession.Query[ParamName], 0);
end;

// Aux function used internaly by ExtToPascal to override HandleEvent method
function TExtObject.ParamAsDouble(ParamName : string) : double; begin
  Result := StrToFloatDef(CurrentWebSession.Query[ParamName], 0);
end;

// Aux function used internaly by ExtToPascal to override HandleEvent method
function TExtObject.ParamAsBoolean(ParamName : string) : boolean; begin
  Result := CurrentWebSession.Query[ParamName] = 'true';
end;

// Aux function used internaly by ExtToPascal to override HandleEvent method
function TExtObject.ParamAsString(ParamName : string) : string; begin
  Result := CurrentWebSession.Query[ParamName];
end;

// Aux function used internaly by ExtToPascal to override HandleEvent method
function TExtObject.ParamAsTDateTime(ParamName : string) : TDateTime; begin
  Result := ParamAsDouble(ParamName);
end;

// Aux function used internaly by ExtToPascal to override HandleEvent method
function TExtObject.ParamAsObject(ParamName : string) : TExtObject; begin
  Result := TExtObject(TExtThread(CurrentWebSession).GarbageFind(CurrentWebSession.Query[ParamName]));
end;

begin
  ExtUtilTextMetrics.FJSName := 'TextMetrics';
  {$IF RTLVersion > 21}
  _JSFormatSettings := TFormatSettings.Create;
  {$ELSE}
  GetLocaleFormatSettings(GetThreadLocale, _JSFormatSettings);
  {$IFEND}
  _JSFormatSettings.DecimalSeparator := '.';

end.
