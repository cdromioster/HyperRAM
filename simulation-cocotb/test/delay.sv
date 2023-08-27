module wiredelay2 (
   inout logic A,
   inout logic B
);
    
   time G_DELAY = 1 ns; // Adjust the delay as needed

   reg ThenTime_v;

   initial begin
      ThenTime_v = 0;
      forever begin
         wait(A, B) until (ThenTime_v != $time);
         // Break
         if (A !== 'z) begin
            #G_DELAY; // wire delay
         end else begin
            #G_DELAY;
         end
         ThenTime_v = $time;
         A = 'z;
         B = 'z;
         #0;

         // Make
         A = B;
         B = A;
      end
   end

endmodule
