inherited SendEmailToolDesignerFrame: TSendEmailToolDesignerFrame
  Width = 458
  Height = 474
  HelpKeyword = 'SendEmailTool'
  ExplicitWidth = 458
  ExplicitHeight = 474
  inherited ClientPanel: TPanel
    Width = 458
    Height = 474
    ExplicitWidth = 458
    ExplicitHeight = 474
    inherited DesignPanel: TPanel
      Width = 458
      Height = 474
      ExplicitWidth = 458
      ExplicitHeight = 474
      inherited ControllerGroupBox: TGroupBox
        Width = 458
        Height = 474
        ExplicitWidth = 458
        ExplicitHeight = 474
        inherited ToolGroupBox: TGroupBox
          Width = 454
          ExplicitWidth = 454
          DesignSize = (
            454
            60)
        end
        inherited DataToolGroupBox: TGroupBox
          Width = 454
          ExplicitWidth = 454
          DesignSize = (
            454
            60)
          inherited _AutoRefresh: TComboBox
            Width = 322
            ExplicitWidth = 322
          end
          inherited _RequireSelection: TCheckBox
            Left = 334
            ExplicitLeft = 334
          end
        end
        object SendEmailToolGroupBox: TGroupBox
          Left = 2
          Top = 135
          Width = 454
          Height = 337
          Align = alClient
          Caption = 'SendEmail'
          TabOrder = 2
          object ServerPageControl: TPageControl
            Left = 2
            Top = 15
            Width = 450
            Height = 320
            ActivePage = SMTPTabSheet
            Align = alClient
            TabOrder = 0
            object SMTPTabSheet: TTabSheet
              Caption = 'Server'
              ImageIndex = 1
              ExplicitLeft = 0
              ExplicitTop = 0
              ExplicitWidth = 0
              ExplicitHeight = 0
              object SMTPPanel: TPanel
                Left = 0
                Top = 0
                Width = 442
                Height = 292
                Align = alClient
                BevelOuter = bvNone
                Padding.Top = 40
                TabOrder = 0
                object SMTPLabel: TLabel
                  Left = 2
                  Top = 0
                  Width = 26
                  Height = 13
                  Caption = 'SMTP'
                end
                object _SMTP: TComboBox
                  Left = 2
                  Top = 16
                  Width = 159
                  Height = 21
                  TabOrder = 0
                  Text = '_SMTP'
                  OnChange = _SMTPChange
                end
                object SMTPHostGroupBox: TGroupBox
                  Left = 0
                  Top = 40
                  Width = 442
                  Height = 252
                  Align = alClient
                  Caption = 'Configuration in Config.yaml'
                  TabOrder = 1
                end
              end
            end
            object MessageTabSheet: TTabSheet
              Caption = 'Message'
              ExplicitLeft = 0
              ExplicitTop = 0
              ExplicitWidth = 0
              ExplicitHeight = 0
            end
          end
        end
      end
    end
  end
end
