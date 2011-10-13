unit Kitto.Types;

interface

uses
  EF.Types;

const
  DEFAULT_PAGE_RECORD_COUNT = 100;

type
  TKEditMode = (emNewRecord, emEditCurrentRecord);

  EKError = class(EEFError);

implementation

end.
