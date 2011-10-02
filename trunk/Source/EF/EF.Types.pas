unit EF.Types;

{$I EF.Defines.inc}

interface

uses
  SysUtils, Generics.Collections;

type
  {
    Many components in EF have OnLog events of this type.
  }
  TEFLogEvent = procedure (const ASender: TObject; const AString: string;
    const ALogLevel: Integer = 1) of object;

  {
    Base class for all EF exceptions.
  }
  EEFError = class(Exception)
  private
    FAdditionalInfo: string;
  public
    constructor CreateWithAdditionalInfo(const AMessage, AAdditionalInfo: string);
    {
      An EF exception may optionally have additional information over what's
      displayed in the Message. The value of this property is set upon
      creation through the CreatEFithAdditionalInfo constructor.
    }
    property AdditionalInfo: string read FAdditionalInfo;
  end;

  TEFStringArray = array of string;

  TEFPair = TPair<string, string>;
  TEFPairs = array of TEFPair;

implementation

{ EEFError }

constructor EEFError.CreateWithAdditionalInfo(const AMessage,
  AAdditionalInfo: string);
begin
  inherited Create(AMessage);
  FAdditionalInfo := AAdditionalInfo;
end;

end.

