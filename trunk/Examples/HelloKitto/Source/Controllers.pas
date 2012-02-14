unit Controllers;

interface

uses
  Kitto.Ext.Controller, Kitto.Ext.DataTool, Kitto.Ext.Base;

type
  TURLToolController = class(TKExtDataToolController)
  protected
    procedure ExecuteTool; override;
  end;

  TTestToolController = class(TKExtDataToolController)
  protected
    procedure ExecuteTool; override;
  published
    procedure Callback;
    procedure DownloadFile;
  end;

implementation

uses
  SysUtils, Classes,
  Ext,
  Kitto.Ext.Session;

{ TTestToolController }

procedure TTestToolController.Callback;
begin
  ExtMessageBox.Alert('Test Tool', 'This is a callback');
end;

procedure TTestToolController.ExecuteTool;
begin
  inherited;
  //ExtMessageBox.Alert('Test Tool', 'This is a custom action', RequestDownload(DownloadFile));
  Download(DownloadFile);
end;

procedure TTestToolController.DownloadFile;
var
  LStream: TFileStream;
begin
  LStream := TFileStream.Create('c:\temp\test2.pdf', fmOpenRead);
  try
    Session.DownloadStream(LStream, 'customfile.pdf');
  finally
    FreeAndNil(LStream);
  end;
  //Session.DownloadFile('c:\temp\test2.pdf');
end;

{ TURLToolController }

procedure TURLToolController.ExecuteTool;
var
  LAddr: string;
begin
  inherited;
  LAddr := Session.RequestHeader['REMOTE_ADDR'];
  if LAddr = '127.0.0.1' then
    Session.Navigate('http://www.ethea.it')
  else
    Session.Navigate('http://www.sencha.com');
end;

initialization
  TKExtControllerRegistry.Instance.RegisterClass('TestTool', TTestToolController);
  TKExtControllerRegistry.Instance.RegisterClass('URLTool', TURLToolController);

finalization
  TKExtControllerRegistry.Instance.UnregisterClass('TestTool');
  TKExtControllerRegistry.Instance.UnregisterClass('URLTool');

end.
