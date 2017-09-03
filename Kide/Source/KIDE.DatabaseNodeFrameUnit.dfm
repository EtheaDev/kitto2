inherited DatabaseNodeFrame: TDatabaseNodeFrame
  Width = 417
  Height = 233
  HelpKeyword = 'Config_Databases'
  ExplicitWidth = 417
  ExplicitHeight = 233
  inherited ClientPanel: TPanel
    Width = 417
    Height = 233
    ExplicitWidth = 417
    ExplicitHeight = 233
    inherited DesignPanel: TPanel
      Width = 417
      Height = 233
      ExplicitWidth = 417
      ExplicitHeight = 233
      object ConnectionTypeEdit: TLabeledEdit
        Left = 215
        Top = 17
        Width = 193
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        EditLabel.Width = 81
        EditLabel.Height = 13
        EditLabel.Caption = 'Connection Type'
        ReadOnly = True
        TabOrder = 1
      end
      object ConnectionGroupBox: TGroupBox
        Left = 7
        Top = 44
        Width = 401
        Height = 181
        Anchors = [akLeft, akTop, akRight, akBottom]
        Caption = 'Connection'
        TabOrder = 2
      end
      object ConnectionNameEdit: TLabeledEdit
        Left = 7
        Top = 17
        Width = 202
        Height = 21
        EditLabel.Width = 84
        EditLabel.Height = 13
        EditLabel.Caption = 'Connection Name'
        TabOrder = 0
      end
    end
  end
end
