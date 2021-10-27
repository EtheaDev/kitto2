{-------------------------------------------------------------------------------
   Copyright 2012-2021 Ethea S.r.l.

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

unit Kitto.Web.Routes;

interface

uses
  Generics.Collections
  , EF.ObserverIntf
  , Kitto.Web.Request
  , Kitto.Web.Response
  , Kitto.Web.URL
  ;

type
  TKWebRoute = class;

  IKWebHandleRequestEventListener = interface
    procedure BeforeHandleRequest(const ASender: TKWebRoute; const ARequest: TKWebRequest;
      const AResponse: TKWebResponse; const AURL: TKWebURL; var AIsAllowed: Boolean);
    procedure AfterHandleRequest(const ASender: TKWebRoute; const ARequest: TKWebRequest;
      const AResponse: TKWebResponse; const AURL: TKWebURL; const AIsFatalError: Boolean);
  end;

  TKWebRouteList = class;

  /// <summary>
  ///  Abstract route. Handles requests for a class of paths.
  /// </summary>
  TKWebRoute = class(TEFSubjectAndObserver)
  private
    FSubscribers: TList<IKWebHandleRequestEventListener>;
    FIsFatalError: Boolean;
  strict protected
    procedure BeforeHandleRequest(const ARequest: TKWebRequest; const AResponse: TKWebResponse;
      const AURL: TKWebURL; var AIsAllowed: Boolean); virtual;
    procedure AfterHandleRequest(const ARequest: TKWebRequest; const AResponse: TKWebResponse;
      const AURL: TKWebURL; const AIsFatalError: Boolean); virtual;
    procedure SignalFatalError;
    function DoHandleRequest(const ARequest: TKWebRequest; const AResponse: TKWebResponse; const AURL: TKWebURL): Boolean; virtual; abstract;
  protected
    // This is meant to be accessed by TKWebRouteList that needs to determine if
    // any of its contained routes fired an error.
    property IsError: Boolean read FIsFatalError;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
  public
    procedure AddSubscriber(const ASubscriber: IKWebHandleRequestEventListener);
    procedure RemoveSubscriber(const ASubscriber: IKWebHandleRequestEventListener);
    /// <summary>
    ///  Handles a request; if handled, returns True (and sets the reponse up) if handled.
    /// </summary>
    function HandleRequest(const ARequest: TKWebRequest; const AResponse: TKWebResponse; const AURL: TKWebURL): Boolean;
    /// <summary>
    ///  Called when the route is added to a list. May be used to add dependant routes.
    ///  The predefined implementation does nothing.
    /// </summary>
    procedure AddedTo(const AList: TKWebRouteList; const AIndex: Integer); virtual;
  end;

  /// <summary>
  ///  Base class for routes that serve local files from the file system.
  /// </summary>
  TKBaseStaticWebRoute = class(TKWebRoute)
  protected
    function ComputeLocalFileName(const ALocalPath, AURLPath, AURLDocument: string): string;
    function ServeLocalFile(const AFileName: string; const AResponse: TKWebResponse): Boolean;
  end;

  /// <summary>
  ///  A route that serves requests for paths matching a specified pattern,
  ///  providing local files from the file system.
  /// </summary>
  TKStaticWebRoute = class(TKBaseStaticWebRoute)
  private
    FPattern: string;
    FPath: string;
    function StripBasePath(const AURLPath: string): string;
  protected
    function DoHandleRequest(const ARequest: TKWebRequest; const AResponse: TKWebResponse;
      const AURL: TKWebURL): Boolean; override;
  public
    constructor Create(const APattern, APath: string);
  end;

  /// <summary>
  ///  A route that serves requests for paths matching a specified pattern,
  ///  looking through an ordered list of local paths and serving the first file
  ///  it finds.
  /// </summary>
  TKMultipleStaticWebRoute = class(TKBaseStaticWebRoute)
  private
    FBasePath: string;
    FLocalPaths: TArray<string>;
    function StripBasePath(const AURLPath: string): string;
  protected
    function DoHandleRequest(const ARequest: TKWebRequest;
      const AResponse: TKWebResponse; const AURL: TKWebURL): Boolean; override;
  public
    constructor Create(const ABasePath: string; const ALocalPaths: TArray<string>);
  end;

  /// <summary>
  ///  A route that owns a list of routes and handles a request by passing it to its contained
  ///  routes until one of them supports and handles it.
  /// </summary>
  TKWebRouteList = class(TKWebRoute)
  private
    FRoutes: TObjectList<TKWebRoute>;
  protected
    function DoHandleRequest(const ARequest: TKWebRequest;
      const AResponse: TKWebResponse; const AURL: TKWebURL): Boolean; override;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
  public
    function AddRoute(const ARoute: TKWebRoute; const AIndex: Integer = -1): TKWebRoute;
  end;

implementation

uses
  SysUtils
  , Classes
  , IOUtils
  , EF.StrUtils
  , Kitto.Web.Types
  ;

{ TKBaseStaticWebRoute }

function TKBaseStaticWebRoute.ComputeLocalFileName(const ALocalPath, AURLPath, AURLDocument: string): string;
var
  LURLPath: string;
begin
  LURLPath := AURLPath.Replace('/', PathDelim);
  Result := TPath.Combine(TPath.Combine(ALocalPath, LURLPath), AURLDocument);
end;

function TKBaseStaticWebRoute.ServeLocalFile(const AFileName: string; const AResponse: TKWebResponse): Boolean;
begin
  if FileExists(AFileName) then
  begin
    AResponse.ContentType := GetFileMimeType(AFileName, 'application/octet-stream');
    AResponse.ReplaceContentStream(TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone));
    Result := True;
  end
  else
    Result := False;
end;

{ TKStaticWebRoute }

constructor TKStaticWebRoute.Create(const APattern, APath: string);
begin
  inherited Create;
  FPattern := APattern;
  FPath := APath;
end;

function TKStaticWebRoute.StripBasePath(const AURLPath: string): string;
var
  LBasePath: string;
begin
  // This only works reliably in the basic case of path/*, which is all we need ATM.
  LBasePath := StripJollyCharacters(FPattern);
  Result := StripPrefix(AURLPath, LBasePath);
end;

function TKStaticWebRoute.DoHandleRequest(const ARequest: TKWebRequest;
  const AResponse: TKWebResponse; const AURL: TKWebURL): Boolean;
var
  LFileName: string;
begin
  Result := False;
  if StrMatches(AURL.Path, FPattern) then
  begin
    LFileName := ComputeLocalFileName(FPath, StripBasePath(AURL.Path), AURL.Document);
    Result := ServeLocalFile(LFileName, AResponse);
  end;
end;

{ TKWebRoute }

procedure TKWebRoute.AddedTo(const AList: TKWebRouteList; const AIndex: Integer);
begin
end;

procedure TKWebRoute.AddSubscriber(const ASubscriber: IKWebHandleRequestEventListener);
begin
  FSubscribers.Add(ASubscriber);
end;

procedure TKWebRoute.AfterConstruction;
begin
  inherited;
  FSubscribers := TList<IKWebHandleRequestEventListener>.Create;
end;

procedure TKWebRoute.AfterHandleRequest(const ARequest: TKWebRequest;
  const AResponse: TKWebResponse; const AURL: TKWebURL; const AIsFatalError: Boolean);
var
  LSubscriber: IKWebHandleRequestEventListener;
begin
  for LSubscriber in FSubscribers do
    LSubscriber.AfterHandleRequest(Self, ARequest, AResponse, AURL, AIsFatalError);
end;

procedure TKWebRoute.BeforeHandleRequest(const ARequest: TKWebRequest;
  const AResponse: TKWebResponse; const AURL: TKWebURL; var AIsAllowed: Boolean);
var
  LSubscriber: IKWebHandleRequestEventListener;
begin
  for LSubscriber in FSubscribers do
    LSubscriber.BeforeHandleRequest(Self, ARequest, AResponse, AURL, AIsAllowed);
end;

destructor TKWebRoute.Destroy;
begin
  FreeAndNil(FSubscribers);
  inherited;
end;

function TKWebRoute.HandleRequest(const ARequest: TKWebRequest;
  const AResponse: TKWebResponse; const AURL: TKWebURL): Boolean;
var
  LIsAllowed: Boolean;
begin
  Result := False;
  LIsAllowed := True;
  FIsFatalError := False;
  BeforeHandleRequest(ARequest, AResponse, AURL, LIsAllowed);
  try
    try
      if LIsAllowed then
        Result := DoHandleRequest(ARequest, AResponse, AURL);
    except
      SignalFatalError;
      raise;
    end;
  finally
    AfterHandleRequest(ARequest, AResponse, AURL, FIsFatalError);
  end;
end;

procedure TKWebRoute.RemoveSubscriber(const ASubscriber: IKWebHandleRequestEventListener);
begin
  FSubscribers.Remove(ASubscriber);
end;

procedure TKWebRoute.SignalFatalError;
begin
  FIsFatalError := True;
end;

{ TKMultipleStaticWebRoute }

constructor TKMultipleStaticWebRoute.Create(const ABasePath: string; const ALocalPaths: TArray<string>);
begin
  inherited Create;
  FBasePath := ABasePath;
  FLocalPaths := RemoveDuplicates(ALocalPaths);
end;

function TKMultipleStaticWebRoute.StripBasePath(const AURLPath: string): string;
begin
  Result := StripPrefix(AURLPath, FBasePath);
end;

function TKMultipleStaticWebRoute.DoHandleRequest(const ARequest: TKWebRequest;
  const AResponse: TKWebResponse; const AURL: TKWebURL): Boolean;
var
  LLocalPath: string;
  LFileName: string;
begin
  Result := False;
  if AURL.Path.StartsWith(FBasePath) then
  begin
    for LLocalPath in FLocalPaths do
    begin
      LFileName := ComputeLocalFileName(LLocalPath, StripBasePath(AURL.Path), AURL.Document);
      Result := ServeLocalFile(LFileName, AResponse);
      if Result then
        Break;
    end;
  end;
end;

{ TKWebRouteList }

function TKWebRouteList.AddRoute(const ARoute: TKWebRoute; const AIndex: Integer): TKWebRoute;
var
  LIndex: Integer;
begin
  if AIndex = -1 then
    LIndex := FRoutes.Add(ARoute)
  else
  begin
    FRoutes.Insert(AIndex, ARoute);
    LIndex := AIndex;
  end;
  ARoute.AddedTo(Self, LIndex);
  Result := ARoute;
end;

function TKWebRouteList.DoHandleRequest(const ARequest: TKWebRequest;
  const AResponse: TKWebResponse; const AURL: TKWebURL): Boolean;
var
  LRoute: TKWebRoute;
begin
  Result := False;
  for LRoute in FRoutes do
  begin
    Result := LRoute.HandleRequest(ARequest, AResponse, AURL);
    // Bubble up errors in the contained routes.
    if LRoute.IsError then
      SignalFatalError;
    if Result then
      Break;
  end;
end;

procedure TKWebRouteList.AfterConstruction;
begin
  inherited;
  FRoutes := TObjectList<TKWebRoute>.Create;
end;

destructor TKWebRouteList.Destroy;
begin
  FreeAndNil(FRoutes);
  inherited;
end;

end.
