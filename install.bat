go build -o p2prc.exe

setx PATH "%PATH%;%cd%"
setx P2PRC "%cd%"

p2prc --dc