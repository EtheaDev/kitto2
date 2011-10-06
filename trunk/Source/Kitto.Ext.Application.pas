unit Kitto.Ext.Application;

{$I Kitto.Defines.inc}

interface

uses
  Classes,
  FCGIApp;

type
  TKExtAppThread = class(TThread)
  private
    FAppTitle: string;
    FTCPPort: Integer;
    FSessionTimeout: Integer;
  protected
    procedure Execute; override;
  public
    destructor Destroy; override;

    property AppTitle: string read FAppTitle write FAppTitle;
    property TCPPort: Integer read FTCPPort write FTCPPort;
    property SessionTimeout: Integer read FSessionTimeout write FSessionTimeout;
  end;

implementation

uses
  SysUtils,
  Kitto.Ext.Session;

{ TKExtAppThread }

destructor TKExtAppThread.Destroy;
begin
  FreeAndNil(Application);
  inherited;
end;

procedure TKExtAppThread.Execute;
begin
  FreeAndNil(Application);
  Application := CreateWebApplication(FAppTitle, TKExtSession, FTCPPort, FSessionTimeout);
  Application.Run(Self);
end;

end.
