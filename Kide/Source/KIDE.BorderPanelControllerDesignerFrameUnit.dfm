inherited BorderPanelControllerDesignerFrame: TBorderPanelControllerDesignerFrame
  Height = 402
  HelpKeyword = 'BorderPanel'
  ExplicitHeight = 402
  inherited ClientPanel: TPanel
    Height = 402
    ExplicitHeight = 402
    inherited DesignPanel: TPanel
      Height = 402
      ExplicitHeight = 402
      inherited ControllerGroupBox: TGroupBox
        Height = 402
        ExplicitHeight = 402
        object ControllerPageControl: TPageControl
          Left = 2
          Top = 72
          Width = 451
          Height = 328
          ActivePage = SubControllersTabSheet
          Align = alClient
          TabOrder = 1
          object SubControllersTabSheet: TTabSheet
            Caption = 'SubControllers'
            ImageIndex = 1
            object SubControllersPageControl: TPageControl
              Left = 0
              Top = 101
              Width = 443
              Height = 199
              ActivePage = CenterControllerTabSheet
              Align = alClient
              TabOrder = 0
              Visible = False
              object CenterControllerTabSheet: TTabSheet
                Caption = 'CenterController'
              end
              object NorthControllerTabSheet: TTabSheet
                Caption = 'NorthController'
                ImageIndex = 2
              end
              object EastControllerTabSheet: TTabSheet
                Caption = 'EastController'
                ImageIndex = 3
              end
              object SouthControllerTabSheet: TTabSheet
                Caption = 'SouthController'
                ImageIndex = 4
              end
              object WestControllerTabSheet: TTabSheet
                Caption = 'WestController'
                ImageIndex = 1
              end
            end
            object ControllersGroupBox: TGroupBox
              Left = 0
              Top = 0
              Width = 443
              Height = 101
              Align = alTop
              Caption = 'Active SubControllers'
              Enabled = False
              TabOrder = 1
              object NorthControllerButton: TSpeedButton
                Left = 2
                Top = 15
                Width = 439
                Height = 28
                Align = alTop
                AllowAllUp = True
                GroupIndex = 1
                Caption = 'NorthController'
                OnClick = SpeedButtonClick
                ExplicitTop = 9
              end
              object WestControllerButton: TSpeedButton
                Left = 2
                Top = 43
                Width = 100
                Height = 28
                Align = alLeft
                AllowAllUp = True
                GroupIndex = 2
                Caption = 'WestController'
                OnClick = SpeedButtonClick
                ExplicitTop = 41
                ExplicitHeight = 35
              end
              object EastControllerButton: TSpeedButton
                Left = 341
                Top = 43
                Width = 100
                Height = 28
                Align = alRight
                AllowAllUp = True
                GroupIndex = 4
                Caption = 'EastController'
                OnClick = SpeedButtonClick
                ExplicitLeft = 378
                ExplicitTop = 41
                ExplicitHeight = 35
              end
              object SouthControllerButton: TSpeedButton
                Left = 2
                Top = 71
                Width = 439
                Height = 28
                Align = alBottom
                AllowAllUp = True
                GroupIndex = 5
                Caption = 'SouthController'
                OnClick = SpeedButtonClick
                ExplicitTop = 79
              end
              object CenterControllerButton: TSpeedButton
                Left = 102
                Top = 43
                Width = 239
                Height = 28
                Align = alClient
                AllowAllUp = True
                GroupIndex = 3
                Caption = 'CenterController'
                OnClick = SpeedButtonClick
                ExplicitLeft = 96
                ExplicitTop = 47
                ExplicitWidth = 243
                ExplicitHeight = 35
              end
            end
          end
          object SubViewsTabSheet: TTabSheet
            Caption = 'SubViews'
            ImageIndex = 2
            object SubViewsPageControl: TPageControl
              Left = 0
              Top = 101
              Width = 443
              Height = 199
              ActivePage = CenterViewTabSheet
              Align = alClient
              TabOrder = 0
              Visible = False
              object CenterViewTabSheet: TTabSheet
                Caption = 'CenterView'
              end
              object NorthViewTabSheet: TTabSheet
                Caption = 'NorthView'
                ImageIndex = 2
              end
              object EastViewTabSheet: TTabSheet
                Caption = 'EastView'
                ImageIndex = 3
              end
              object SouthViewTabSheet: TTabSheet
                Caption = 'SouthView'
                ImageIndex = 4
              end
              object WestViewTabSheet: TTabSheet
                Caption = 'WestView'
                ImageIndex = 1
              end
            end
            object ViewsGroupBox: TGroupBox
              Left = 0
              Top = 0
              Width = 443
              Height = 101
              Align = alTop
              Caption = 'Active SubViews'
              Enabled = False
              TabOrder = 1
              ExplicitTop = -6
              object NorthViewButton: TSpeedButton
                Left = 2
                Top = 15
                Width = 439
                Height = 28
                Align = alTop
                AllowAllUp = True
                GroupIndex = 1
                Caption = 'NorthView'
                OnClick = SpeedButtonClick
              end
              object WestViewButton: TSpeedButton
                Left = 2
                Top = 43
                Width = 100
                Height = 28
                Align = alLeft
                AllowAllUp = True
                GroupIndex = 2
                Caption = 'WestView'
                OnClick = SpeedButtonClick
                ExplicitTop = 41
                ExplicitHeight = 35
              end
              object EastViewButton: TSpeedButton
                Left = 341
                Top = 43
                Width = 100
                Height = 28
                Align = alRight
                AllowAllUp = True
                GroupIndex = 4
                Caption = 'EastView'
                OnClick = SpeedButtonClick
                ExplicitLeft = 378
                ExplicitTop = 41
                ExplicitHeight = 35
              end
              object SouthViewButton: TSpeedButton
                Left = 2
                Top = 71
                Width = 439
                Height = 28
                Align = alBottom
                AllowAllUp = True
                GroupIndex = 5
                Caption = 'SouthView'
                OnClick = SpeedButtonClick
                ExplicitTop = 99
              end
              object CenterViewButton: TSpeedButton
                Left = 102
                Top = 43
                Width = 239
                Height = 28
                Align = alClient
                AllowAllUp = True
                GroupIndex = 3
                Caption = 'CenterView'
                OnClick = SpeedButtonClick
                ExplicitLeft = 77
                ExplicitTop = 41
                ExplicitWidth = 280
                ExplicitHeight = 35
              end
            end
          end
        end
      end
    end
  end
  inherited ActionList: TActionList
    Left = 360
    Top = 136
  end
end
