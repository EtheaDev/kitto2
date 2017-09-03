inherited WaitForm: TWaitForm
  BorderStyle = bsNone
  ClientHeight = 65
  ClientWidth = 637
  FormStyle = fsStayOnTop
  Position = poOwnerFormCenter
  OnDeactivate = FormDeactivate
  ExplicitWidth = 637
  ExplicitHeight = 65
  DesignSize = (
    637
    65)
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel: TBevel
    Left = 0
    Top = 0
    Width = 636
    Height = 65
    Anchors = [akLeft, akTop, akRight, akBottom]
    Style = bsRaised
  end
  object WaitMessageLabel: TLabel
    Left = 8
    Top = 26
    Width = 89
    Height = 13
    Alignment = taCenter
    Caption = 'WaitMessageLabel'
  end
end
