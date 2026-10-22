#!/bin/bash
#ChrysaLisp for iOS. Makes the Xcode project, in mobile/build/ios, and
#builds it for the simulator, or with -d for a device, signed by a team.
#
#mobile/ios/build.sh [-d TEAM_ID] [-r]	-r runs it on a booted simulator
#
#The project can be opened in Xcode as well, to run it on a device:
#open mobile/build/ios/ChrysaLisp.xcodeproj

set -e
cd "$(dirname "$0")/.."
mobile=$(pwd)

device=""
team=""
run=""
while [ "$#" -gt 0 ]; do
	case $1 in
		-d) device=1; team=$2; shift 2 ;;
		-r) run=1; shift ;;
		*) echo "mobile/ios/build.sh [-d TEAM_ID] [-r]"; exit 1 ;;
	esac
done

./fetch_sdl.sh
./bundle.sh build/assets/chrysalisp.tar

cmake -S . -B build/ios -G Xcode \
	-DCMAKE_SYSTEM_NAME=iOS \
	-DCMAKE_OSX_DEPLOYMENT_TARGET=15.0 \
	-DCL_BUNDLE="$mobile/build/assets/chrysalisp.tar" \
	-DCL_TEAM="$team"

if [ "$device" == "" ]
then
	xcodebuild -project build/ios/ChrysaLisp.xcodeproj -target main -configuration Release \
		-sdk iphonesimulator -arch arm64 CODE_SIGNING_ALLOWED=NO -quiet
	app="$mobile/build/ios/Release-iphonesimulator/ChrysaLisp.app"
else
	xcodebuild -project build/ios/ChrysaLisp.xcodeproj -target main -configuration Release \
		-sdk iphoneos -allowProvisioningUpdates -quiet
	app="$mobile/build/ios/Release-iphoneos/ChrysaLisp.app"
fi
echo "Built $app"

if [ "$run" != "" ]
then
	if [ "$device" == "" ]
	then
		xcrun simctl install booted "$app"
		xcrun simctl launch --console-pty booted org.chrysalisp.app
	else
		xcrun devicectl device install app --device "${CL_DEVICE:?set CL_DEVICE to the device id, xcrun devicectl list devices}" "$app"
		xcrun devicectl device process launch --device "$CL_DEVICE" org.chrysalisp.app
	fi
fi
