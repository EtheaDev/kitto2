{-------------------------------------------------------------------------------
   Copyright 2015 Ethea S.r.l.

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

/// <summary>
///  Base class for lookup editors and filters.
/// </summary>
unit Kitto.Ext.LookupField;

interface

uses
  SysUtils,
  ExtPascal, ExtPascalUtils, ExtForm,
  EF.Intf, EF.ObserverIntf,
  Kitto.Metadata.Views, Kitto.Metadata.DataView,
  Kitto.Ext.Controller;

type
  TKExtLookupField = class(TExtFormTwinTriggerField,  IInterface, IEFInterface, IEFSubject, IEFObserver)
  private
    FSubjObserverImpl: TEFSubjectAndObserver;
    FLookupController: IKExtController;
    FViewField: TKViewField;
    function GetClickJSCode(const AMethod: TExtProcedure): string;
  protected
    procedure InitDefaults; override;
  strict protected
    procedure LookupConfirmed(const ARecord: TKViewTableRecord); virtual; abstract;
    class function FindLookupView(const AViewField: TKViewField): TKView;
    procedure SetViewField(const AValue: TKViewField);
    function GetViewField: TKViewField;
    property ViewField: TKViewField read FViewField;
  public
    destructor Destroy; override;
    function AsObject: TObject; inline;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure AttachObserver(const AObserver: IEFObserver); virtual;
    procedure DetachObserver(const AObserver: IEFObserver); virtual;
    procedure NotifyObservers(const AContext: string = ''); virtual;
    procedure UpdateObserver(const ASubject: IEFSubject; const AContext: string); virtual;
    function AsExtObject: TExtObject;
    class function SupportsViewField(const AViewField: TKViewField): Boolean; static;
  published
    procedure TriggerClick;
    procedure ClearClick; virtual;
  end;

implementation

uses
  StrUtils,
  EF.StrUtils, EF.Localization,
  Kitto.Metadata, Kitto.Config,
  Kitto.Ext.Session;

{ TKExtLookupField }

function TKExtLookupField.AsExtObject: TExtObject;
begin
  Result := Self;
end;

function TKExtLookupField.AsObject: TObject;
begin
  Result := Self;
end;

procedure TKExtLookupField.AttachObserver(const AObserver: IEFObserver);
begin
  FSubjObserverImpl.AttachObserver(AObserver);
end;

destructor TKExtLookupField.Destroy;
begin
  FreeAndNil(FSubjObserverImpl);
  inherited;
end;

procedure TKExtLookupField.DetachObserver(const AObserver: IEFObserver);
begin
  FSubjObserverImpl.DetachObserver(AObserver);
end;

class function TKExtLookupField.FindLookupView(const AViewField: TKViewField): TKView;
begin
  Result := AViewField.Table.View.Catalog.FindObjectByPredicate(
    function (const AObject: TKMetadata): Boolean
    begin
      Result := (AObject is TKDataView) and AObject.GetBoolean('IsLookup')
        and (TKDataView(AObject).MainTable.Model = AViewField.ModelField.ReferencedModel);
    end) as TKView;
end;

function TKExtLookupField.GetViewField: TKViewField;
begin
  Result := FViewField;
end;

procedure TKExtLookupField.InitDefaults;
begin
  inherited;
  FSubjObserverImpl := TEFSubjectAndObserver.Create;
  Editable := False;
  Trigger1Class := 'x-form-search-trigger';
  Trigger2Class := 'x-form-clear-trigger';
end;

procedure TKExtLookupField.NotifyObservers(const AContext: string);
begin
  FSubjObserverImpl.NotifyObserversOnBehalfOf(Self, AContext);
end;

procedure TKExtLookupField.UpdateObserver(const ASubject: IEFSubject; const AContext: string);
var
  LRecord: TKViewTableRecord;
begin
  if ASubject.AsObject = FLookupController.AsObject then
  begin
    if MatchText(AContext, ['LookupConfirmed', 'LookupCanceled']) then
    begin
      if SameText(AContext, 'LookupConfirmed') then
      begin
        LRecord := FLookupController.Config.GetObject('Sys/LookupResultRecord') as TKViewTableRecord;
        Assert(Assigned(LRecord));
        LookupConfirmed(LRecord);
      end;
      FreeAndNilEFIntf(FLookupController);
    end;
  end;
end;

function TKExtLookupField._AddRef: Integer;
begin
  Result := -1;
end;

function TKExtLookupField._Release: Integer;
begin
  Result := -1;
end;

procedure TKExtLookupField.TriggerClick;
var
  LView: TKView;
  LSubject: IEFSubject;
begin
  FreeAndNilEFIntf(FLookupController);
  Assert(Assigned(FViewField));

  LView := FindLookupView(FViewField);
  Assert(Assigned(LView));

  FLookupController := Session.DisplayNewController(LView, True,
    procedure (AWindow: TKExtControllerHostWindow)
    begin
      AWindow.Title := _(Format('Choose %s', [FViewField.DisplayLabel]));
    end,
    procedure (AController: IKExtController)
    begin
      AController.Config.SetBoolean('Sys/LookupMode', True);
      AController.Config.SetString('Sys/LookupFilter', FViewField.LookupFilter);
    end);
  if Supports(FLookupController, IEFSubject, LSubject) then
    LSubject.AttachObserver(Self);
end;

procedure TKExtLookupField.ClearClick;
begin
end;

procedure TKExtLookupField.SetViewField(const AValue: TKViewField);
begin
  FViewField := AValue;
  if not ReadOnly then
  begin
    Session.ResponseItems.ExecuteJSCode(Self,
      JSName + '.onTrigger1Click = function(e) { ' + GetClickJSCode(TriggerClick) + '};');
    Session.ResponseItems.ExecuteJSCode(Self,
      JSName + '.onTrigger2Click = function(e) { ' + GetClickJSCode(ClearClick) + '};');
  end;
end;

function TKExtLookupField.GetClickJSCode(const AMethod: TExtProcedure): string;
begin
  Result := GetPOSTAjaxCode(AMethod, [], 'null');
end;

class function TKExtLookupField.SupportsViewField(const AViewField: TKViewField): Boolean;
begin
  Assert(Assigned(AViewField));
  Result := AViewField.IsReference and Assigned(FindLookupView(AViewField));
end;

end.

