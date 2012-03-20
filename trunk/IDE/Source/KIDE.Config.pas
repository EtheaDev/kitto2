unit KIDE.Config;

interface

uses
  Kitto.Config;

type
  TConfig = class(TKConfig)
  public
    function GetAccessGrantValue(const AResourceURI, AMode: string;
      const ADefaultValue: Variant): Variant; override;
  end;

implementation

uses
  Kitto.AccessControl;

{ TConfig }

function TConfig.GetAccessGrantValue(const AResourceURI, AMode: string;
  const ADefaultValue: Variant): Variant;
begin
  Result := ACV_TRUE;
end;

initialization
  TKConfig.SetConfigClass(TConfig);

end.
