inherited ConfigAuthNodeFrame: TConfigAuthNodeFrame
  Width = 655
  Height = 548
  HelpKeyword = 'Config_Auth'
  ExplicitWidth = 655
  ExplicitHeight = 548
  inherited ClientPanel: TPanel
    Width = 655
    Height = 548
    ExplicitWidth = 655
    ExplicitHeight = 548
    inherited DesignPanel: TPanel
      Width = 655
      Height = 548
      ExplicitWidth = 655
      ExplicitHeight = 548
      object AuthDBGroupBox: TGroupBox
        Left = 0
        Top = 172
        Width = 655
        Height = 376
        Align = alClient
        Caption = 'Auth DB: parameters'
        TabOrder = 0
        object AuthDBPanel: TPanel
          Left = 2
          Top = 15
          Width = 651
          Height = 46
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 0
          DesignSize = (
            651
            46)
          object _IsPassepartoutEnabled: TCheckBox
            Left = 2
            Top = 19
            Width = 142
            Height = 17
            Caption = 'IsPassepartoutEnabled'
            TabOrder = 0
          end
          object _PassepartoutPassword: TLabeledEdit
            Left = 152
            Top = 17
            Width = 465
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            EditLabel.Width = 110
            EditLabel.Height = 13
            EditLabel.Caption = 'PassepartoutPassword'
            TabOrder = 1
          end
        end
        object AuthPageControl: TPageControl
          Left = 2
          Top = 61
          Width = 651
          Height = 313
          ActivePage = ReadUserCommandTextTabSheet
          Align = alClient
          TabOrder = 1
          object ReadUserCommandTextTabSheet: TTabSheet
            Caption = 'ReadUserCommandText'
            object ReadUserCommandTextPanel: TPanel
              Left = 0
              Top = 0
              Width = 643
              Height = 285
              Align = alClient
              BevelOuter = bvNone
              TabOrder = 0
            end
          end
          object SetPasswordCommandTextTabSheet: TTabSheet
            Caption = 'SetPasswordCommandText'
            ImageIndex = 1
            object SetPasswordCommandTextPanel: TPanel
              Left = 0
              Top = 0
              Width = 643
              Height = 285
              Align = alClient
              BevelOuter = bvNone
              TabOrder = 0
            end
          end
          object AfterAuthenticateCommandTextTabSheet: TTabSheet
            Caption = 'AfterAuthenticateCommandText'
            ImageIndex = 2
            object AfterAuthenticateCommandTextPanel: TPanel
              Left = 0
              Top = 0
              Width = 643
              Height = 285
              Align = alClient
              BevelOuter = bvNone
              TabOrder = 0
            end
          end
        end
      end
      object AuthTextFileGroupBox: TGroupBox
        Left = 0
        Top = 110
        Width = 655
        Height = 62
        Align = alTop
        Caption = 'Auth TexFile: parameters'
        TabOrder = 1
        DesignSize = (
          655
          62)
        object _FileName: TLabeledEdit
          Left = 6
          Top = 31
          Width = 613
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          EditLabel.Width = 43
          EditLabel.Height = 13
          EditLabel.Caption = 'FileName'
          TabOrder = 0
        end
      end
      object AuthScrollBox: TScrollBox
        Left = 0
        Top = 0
        Width = 655
        Height = 110
        Align = alTop
        BevelInner = bvNone
        BevelOuter = bvNone
        BorderStyle = bsNone
        TabOrder = 2
        object AuthAutoScrollPanel: TPanel
          Left = 0
          Top = 99
          Width = 305
          Height = 11
          Align = alLeft
          BevelOuter = bvNone
          TabOrder = 2
        end
        object AuthPanel: TGroupBox
          Left = 0
          Top = 0
          Width = 655
          Height = 42
          Align = alTop
          Caption = 'Authentication Type'
          TabOrder = 0
          object AuthComboBox: TComboBox
            Left = 4
            Top = 15
            Width = 197
            Height = 21
            DropDownCount = 20
            TabOrder = 0
            OnChange = AuthComboBoxChange
          end
          object _IsClearPassword: TCheckBox
            Left = 208
            Top = 17
            Width = 142
            Height = 17
            Caption = 'IsClearPassword'
            TabOrder = 1
          end
        end
        object DefaultsGroupBox: TGroupBox
          Left = 0
          Top = 42
          Width = 655
          Height = 57
          Align = alTop
          Caption = 'Defaults'
          TabOrder = 1
          object _Defaults_Password: TLabeledEdit
            Left = 132
            Top = 31
            Width = 121
            Height = 21
            EditLabel.Width = 46
            EditLabel.Height = 13
            EditLabel.Caption = 'Password'
            TabOrder = 1
          end
          object _Defaults_UserName: TLabeledEdit
            Left = 5
            Top = 31
            Width = 121
            Height = 21
            EditLabel.Width = 49
            EditLabel.Height = 13
            EditLabel.Caption = 'UserName'
            TabOrder = 0
          end
        end
      end
    end
  end
end
