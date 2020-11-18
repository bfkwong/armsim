make all
make run > fibOutput.txt
diff fibOutput.txt outputs/fib.completeoutput 

make shang0 > shangOutput.txt 
diff shangOutput.txt outputs/shangO0.completeoutput

echo "------------------------------------"
echo "------------------------------------"
echo "------------------------------------"

make shang1 > shangOutput.txt 
diff shangOutput.txt outputs/shang.O1.completeoutput

echo "------------------------------------"
echo "------------------------------------"
echo "------------------------------------"

make shang2 > shangOutput.txt 
diff shangOutput.txt outputs/shang.O2.completeoutput
