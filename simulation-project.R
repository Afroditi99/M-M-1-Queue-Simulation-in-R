save.image("C:\\Users\\EvangelosVassiliou\\OneDrive - chios.aegean.gr\\My_lecture_notes\\aegean university\\simulation\\MM1.RData")

time=0.0

server_status=0
num_in_q=0
time_arrival=c()

time_last_event=0.0
num_custs_delayed=0
total_of_delays=0.0
area_num_in_q=0.0
area_server_status=0.0

time_between_arrivals=c(runif(100))
time_serving=c(runif(100))
time_next_event=c()
time_next_event[1]=time_between_arrivals[1]
time_next_event[2]=time_serving[2]

num_events=2
num_delays_required=100

l=1
k=0
Q_limit=100

# Plots initialization
x=c(0,time_between_arrivals[1])
y=c();
for (i in 1:2){
  y[i]=num_in_q;
}
z=c();
for (i in 1:2){
  z[i]=server_status; 
}


while (num_custs_delayed<num_delays_required){
  # Determine the next event 
  min_time_next_event=10^29
  for (i in 1:num_events){
    if (time_next_event[i]<min_time_next_event){
      min_time_next_event=time_next_event[i]
      next_event_type=i
    }
  }
  for i=90 server_status==1 
  break; 
  time=min_time_next_event 
  # Update time average statistical accumulators
  time_since_last_event=time-time_last_event;

  x=c(x,time);
  time_last_event=time;
  area_num_in_q = area_num_in_q + (num_in_q * time_since_last_event);
  y=c(y,num_in_q)
  
  # graph1=plot(x,y,'r','LineWidth',4)
  # hold on
  area_server_status = area_server_status + (server_status*time_since_last_event);
  z = c(z,server_status)
  # plot(x,z,'r','LineWidth',5)
  # hold on
  # Invoke the appropriate event function
  
  if(next_event_type == 1){
    l=l+1;
    time_next_event[1]=time+time_between_arrivals[l];
    if(server_status==1){
      num_in_q=num_in_q+1;
      if(num_in_q>Q_limit){
        dscskncsknc
      }
      time_arrival[num_in_q]=time;
    }else{
      delay=0.0;
      total_of_delays=total_of_delays+delay;
      num_custs_delayed=num_custs_delayed+1;
      server_status=1;
      k=k+1;
      time_next_event[2]=time+time_serving[k];
    }
  }else{
    if(num_in_q == 0){
      server_status=0;
      time_next_event[2]=10^30;
    }else{
      num_in_q=num_in_q-1;
      delay=time-time_arrival[1];
      total_of_delays=total_of_delays+delay;
      num_custs_delayed=num_custs_delayed+1;
      k=k+1;
      time_next_event[2]=time+time_serving[k];
      for(i in 1:num_in_q){
        time_arrival[i]=time_arrival[i+1];
      }
    }
  }
}

 
# Invoke the report generator and end the simulation
dhat=total_of_delays/num_custs_delayed
qhat=area_num_in_q/time
uhat=area_server_status/time
jhat=(total_time_arrivals/total_time_serving)-1 
# hold off

plot(x,y,xaxt='n',type="S",col='red',lwd=2,main = "Number of Customers in Queue",xlab = "Time")
axis(1, at=seq(0,10,by=0.2), labels=TRUE)
plot(x,z,xaxt='n',type="S",col='red',lwd=2,main = "Server Status",xlab = "time",xlim = c(0, 10))
axis(1, at=seq(0,10,by=0.2), labels=TRUE)


# plot(1:10, xaxt = "n", xlab='Some Letters')
     