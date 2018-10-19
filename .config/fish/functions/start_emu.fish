function start_emu
	cd /home/pierre/Android/Sdk/emulator
	env QT_AUTO_SCREEN_SCALE_FACTOR=0 ./emulator -avd emu -gpu host -accel on -no-boot-anim -memory 4096
	cd -
end
