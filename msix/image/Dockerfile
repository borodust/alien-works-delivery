# escape=`

FROM mcr.microsoft.com/windows/servercore:ltsc2019

SHELL ["cmd", "/S", "/C"]

#
# INIT WINDOWS BUILD TOOLS
# TODO: MOVE TO MSIX IMAGE
#
RUN `
    # Download the Build Tools bootstrapper.
    curl -SL --output vs_buildtools.exe https://aka.ms/vs/16/release/vs_buildtools.exe `
    `
    # Install Build Tools with the Microsoft.VisualStudio.Workload.AzureBuildTools workload, excluding workloads and components with known issues.
    && (start /w vs_buildtools.exe --quiet --wait --norestart --nocache `
        --installPath "%ProgramFiles(x86)%\Microsoft Visual Studio\2019\BuildTools" `
        --add Microsoft.VisualStudio.Component.Windows10SDK.19041 `
        --remove Microsoft.VisualStudio.Component.Windows10SDK.10240 `
        --remove Microsoft.VisualStudio.Component.Windows10SDK.10586 `
        --remove Microsoft.VisualStudio.Component.Windows10SDK.14393 `
        --remove Microsoft.VisualStudio.Component.Windows81SDK `
        || IF "%ERRORLEVEL%"=="3010" EXIT 0) `
    `
    # Cleanup
    && del /q vs_buildtools.exe

COPY provision.ps1 ./

#
# PROVISION ENVIRONMENT
#
RUN powershell -File ./provision.ps1

COPY build.ps1 init.ps1 init.lisp ./

#
# INITIALIZE ENVIRONMENT
#
RUN powershell -File ./init.ps1


ENTRYPOINT ["powershell", "-File", "./build.ps1"]
