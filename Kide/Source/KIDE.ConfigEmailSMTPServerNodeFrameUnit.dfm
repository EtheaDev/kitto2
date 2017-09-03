inherited ConfigEmailSMTPServerNodeFrame: TConfigEmailSMTPServerNodeFrame
  Width = 374
  ExplicitWidth = 374
  inherited ClientPanel: TPanel
    Width = 374
    ExplicitWidth = 374
    inherited DesignPanel: TPanel
      Width = 374
      ExplicitWidth = 374
      object PortLabel: TLabel
        Left = 8
        Top = 41
        Width = 20
        Height = 13
        Caption = 'Port'
      end
      object _HostName: TLabeledEdit
        Left = 8
        Top = 18
        Width = 360
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        EditLabel.Width = 49
        EditLabel.Height = 13
        EditLabel.Caption = 'HostName'
        TabOrder = 0
      end
      object _UserName: TLabeledEdit
        Left = 8
        Top = 98
        Width = 360
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        EditLabel.Width = 49
        EditLabel.Height = 13
        EditLabel.Caption = 'UserName'
        TabOrder = 3
      end
      object _Password: TLabeledEdit
        Left = 8
        Top = 137
        Width = 360
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        EditLabel.Width = 46
        EditLabel.Height = 13
        EditLabel.Caption = 'Password'
        TabOrder = 4
      end
      object _Port: TSpinEdit
        Left = 8
        Top = 56
        Width = 73
        Height = 22
        MaxValue = 0
        MinValue = 0
        TabOrder = 1
        Value = 0
      end
      object _UseTLS: TCheckBox
        Left = 96
        Top = 58
        Width = 97
        Height = 17
        Caption = 'UseTLS'
        TabOrder = 2
      end
    end
  end
end
