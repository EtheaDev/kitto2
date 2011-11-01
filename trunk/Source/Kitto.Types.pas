unit Kitto.Types;

{$I Kitto.Defines.inc}

interface

uses
  EF.Types;

const
  MAX_RECORD_COUNT = 50;

type
  TKEditMode = (emNewRecord, emEditCurrentRecord);

  EKError = class(EEFError);

implementation

end.
