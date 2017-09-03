inherited LoginWindowLocalStorageDesignerFrame: TLoginWindowLocalStorageDesignerFrame
  HelpKeyword = 'Login'
  inherited ClientPanel: TPanel
    inherited DesignPanel: TPanel
      object ModeLabel: TLabel
        Left = 4
        Top = 2
        Width = 26
        Height = 13
        Caption = 'Mode'
      end
      object _Mode: TComboBox
        Left = 4
        Top = 17
        Width = 145
        Height = 21
        TabOrder = 0
        Items.Strings = (
          'UserName'
          'Password')
      end
      object _AskUser_Default: TCheckBox
        Left = 231
        Top = 19
        Width = 97
        Height = 17
        Caption = 'AskUser/Default'
        TabOrder = 2
      end
      object _AskUser: TCheckBox
        Left = 155
        Top = 19
        Width = 70
        Height = 17
        Caption = 'AskUser'
        TabOrder = 1
      end
    end
  end
end
