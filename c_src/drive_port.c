#include "rc/motor.h"
#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <errno.h>

#define LEFT  1
#define RIGHT 2

int read_command(char*,size_t);
void dispatch(char*);

int read_command(char *buf, size_t len)
{
   char size;
   if(read(0, &size, 1) != 1)
   {
      return -1;
   }

   int read_so_far = 0;
   while(size - read_so_far > 0)
   {
      int result = read(0, buf+read_so_far, size-read_so_far);
      if(result == -1)
      {
         return -1;
      }
      read_so_far += result;
   }
   return size;
}

void dispatch(char *command)
{
   if(command[0] == 'd')
   {
      // then this is a drive command
      float duty_right, duty_left;
      duty_left = duty_right = 0;
      int n = sscanf(&command[1], "%f,%f", &duty_left, &duty_right);
      if(n != 2 && errno != 0) {
         perror("sscanf()");
         return;
      }
      else {
         printf("duty_left: %f\n", duty_left);
         printf("duty_right: %f\n", duty_right);
         rc_motor_set(LEFT, -duty_left);
         rc_motor_set(RIGHT, duty_right);
      }
   }

   else if(command[0] == 'q')
   {
      rc_motor_cleanup();
      exit(0);
   }
   printf("> %s", command);
}

int main(int argc, char **argv)
{
   char command[256];
   int running = 1;
   if(rc_motor_init()) return -1;
   do
   {
      if(read_command(command, 256) <= 0)
      {
         perror("read_command()");
         exit(-1);
      }
      dispatch(command);
   } while(running);
   return 0;
}