all:
	g++ -g main.cpp decode.cpp thumbsim_driver.cpp parse.cpp execute.cpp -o thumbsim

run:
	./thumbsim -i -d -f inputs/fib.sim

shang0:
	./thumbsim -i -d -f inputs/shang.O0.sim

shang1:
	./thumbsim -i -d -f inputs/shang.O1.sim

shang2:
	./thumbsim -i -d -f inputs/shang.O2.sim

clean:
	rm -rf ./*.o ./thumbsim
