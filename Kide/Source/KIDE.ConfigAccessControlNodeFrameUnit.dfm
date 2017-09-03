inherited ConfigAccessControlNodeFrame: TConfigAccessControlNodeFrame
  Width = 671
  Height = 485
  HelpKeyword = 'Config_AccessControl'
  ExplicitWidth = 671
  ExplicitHeight = 485
  inherited ClientPanel: TPanel
    Width = 671
    Height = 485
    ExplicitWidth = 671
    ExplicitHeight = 485
    inherited DesignPanel: TPanel
      Width = 671
      Height = 485
      ExplicitWidth = 671
      ExplicitHeight = 485
      object ACPageControl: TPageControl
        Left = 0
        Top = 42
        Width = 671
        Height = 443
        ActivePage = ReadPermissionsCommandTextTabSheet
        Align = alClient
        TabOrder = 0
        object ReadPermissionsCommandTextTabSheet: TTabSheet
          Caption = 'ReadPermissionsCommandText'
          object ReadPermissionsCommandTextPanel: TPanel
            Left = 0
            Top = 0
            Width = 663
            Height = 415
            Align = alClient
            BevelOuter = bvNone
            TabOrder = 0
          end
        end
        object ReadRolesCommandTextTabSheet: TTabSheet
          Caption = 'ReadRolesCommandText'
          ImageIndex = 1
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
          object ReadRolesCommandTextPanel: TPanel
            Left = 0
            Top = 0
            Width = 663
            Height = 415
            Align = alClient
            BevelOuter = bvNone
            TabOrder = 0
          end
        end
      end
      object ACGroupBox: TGroupBox
        Left = 0
        Top = 0
        Width = 671
        Height = 42
        Align = alTop
        Caption = 'Access Control Type'
        TabOrder = 1
        object ACComboBox: TComboBox
          Left = 4
          Top = 15
          Width = 197
          Height = 21
          DropDownCount = 20
          TabOrder = 0
          OnChange = ACComboBoxChange
        end
      end
    end
  end
end
