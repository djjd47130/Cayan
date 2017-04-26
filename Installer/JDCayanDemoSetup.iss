
#define MyAppName "Cayan Demo"
#define MyAppVersion "1.0"
#define MyAppPublisher "JD Software Inc."
#define MyAppURL "http://www.jerrydodge.com/Cayan"
#define MyAppExeName "CayanPOS.exe"

[Setup]
AppId={{9022B2E4-40EE-4AE7-9AA2-D3029C77DB9D}
AppName={#MyAppName}
AppVersion={#MyAppVersion}
AppVerName={#MyAppName} {#MyAppVersion}
AppPublisher={#MyAppPublisher}
AppPublisherURL={#MyAppURL}
AppSupportURL={#MyAppURL}
AppUpdatesURL={#MyAppURL}
ArchitecturesAllowed=x64
DefaultDirName={pf}\{#MyAppName}
DefaultGroupName={#MyAppName}
DisableProgramGroupPage=yes
LicenseFile=E:\Development\JD\Delphi\Cayan\Installer\License.txt
OutputDir=.\Output
OutputBaseFilename=JDCayanDemoSetup
; SetupIconFile=E:\Development\JD\Delphi\Cayan\Installer\Cayan.ico
Compression=lzma
SolidCompression=yes

[Languages]                      
Name: "english"; MessagesFile: "compiler:Default.isl"

[Tasks]
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"; GroupDescription: "{cm:AdditionalIcons}"; Flags: unchecked

[Files]
Source: "E:\Development\JD\Delphi\Cayan\Bin\CayanPOS.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "E:\Development\JD\Delphi\Cayan\Bin\CayanCEDEmulator.exe"; DestDir: "{app}"; Flags: ignoreversion
; NOTE: Don't use "Flags: ignoreversion" on any shared system files

[Icons]
Name: "{group}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"         
Name: "{group}\CED Emulator"; Filename: "{app}\CayanCEDEmulator.exe"
Name: "{group}\{cm:ProgramOnTheWeb,{#MyAppName}}"; Filename: "{#MyAppURL}"
Name: "{group}\{cm:UninstallProgram,{#MyAppName}}"; Filename: "{uninstallexe}"
Name: "{commondesktop}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"; Tasks: desktopicon     
Name: "{commondesktop}\CED Emulator"; Filename: "{app}\CayanCEDEmulator.exe"; Tasks: desktopicon

[Run]
Filename: "{app}\{#MyAppExeName}"; Description: "{cm:LaunchProgram,{#StringChange(MyAppName, '&', '&&')}}"; Flags: nowait postinstall skipifsilent



[Code]

     
procedure CurStepChanged(CurStep: TSetupStep);
var
  Comps: String;
begin
  {
  case CurStep of
    ssInstall: begin
      if IsServiceInstalled then begin
        DoStopService;
      end;
    end;
    ssPostInstall: begin
      Comps:= WizardSelectedComponents(False);
      if Pos('server', Comps) > 0 then begin 
        DoInstallService;
        DoStartService;
      end else begin
        //If service is installed. stop / uninstall it
        if IsServiceInstalled then begin
          if IsServiceRunning then begin
            DoStopService;
          end;
          DoUninstallService;
        end;
      end;   
    end;
    ssDone: begin

    end;
  end;
  }
end;
                       

procedure CurPageChanged(CurPageID: Integer);
begin
  case CurPageID of
    wpWelcome: begin

    end;
    wpLicense: begin

    end;
    wpPassword: begin

    end;
    wpInfoBefore: begin

    end;
  end;
end;

















