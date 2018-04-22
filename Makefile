
alien:
	ros dynamic-space-size=8000 dump --disable-compression executable roswell/alien.ros -o alien

planner.img: $(shell git ls-files)
	sudo singularity build planner.img ./Singularity

backup: 
	cp planner.img planner0.img

restore:
	sudo cp planner0.img planner.img
	touch Singularity

test: planner.img
	sudo ./singularity-test.sh

clean:
	sudo rm -rf planner.img rundir
