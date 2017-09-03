inherited ConfigEmailSMTPNodeFrame: TConfigEmailSMTPNodeFrame
  Width = 374
  ExplicitWidth = 374
  inherited ClientPanel: TPanel
    Width = 374
    ExplicitWidth = 374
    inherited DesignPanel: TPanel
      Width = 374
      ExplicitWidth = 374
    end
  end
  object SMTPTabControl: TTabControl [1]
    Left = 0
    Top = 0
    Width = 374
    Height = 299
    Align = alClient
    TabOrder = 1
    Tabs.Strings = (
      'Default')
    TabIndex = 0
    OnChange = SMTPTabControlChange
  end
end
