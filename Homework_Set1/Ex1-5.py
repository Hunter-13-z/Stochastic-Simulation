import numpy as np
import heapq
import scipy.special as sp

'''
Function to draw from a Weibull distribution with a mean of 1.25
'''
mean_weibull = 1.25
k = 2
lambda_weibull = mean_weibull / sp.gamma(1 + 1/k)

def draw_weibull(size=1):
    U = np.random.uniform(0, 1, size)
    return lambda_weibull * (-np.log(1 - U))**(1 / k)


'''
Function to draw from Normal(1, 0.1) truncated by 0.5 1.5
'''

def generate_normal(n):
    def generate_geo(n):
        U = np.random.uniform(0, 1, n)
        samples = -np.log(U)
        return samples
    
    Y1 = generate_geo(n)
    Y2 = generate_geo(n)
    
    Z = Y1[Y2 >= (Y1 - 1)**2 / 2]
    
    while len(Z) < n:
        Y1 = generate_geo(n)
        Y2 = generate_geo(n)
        
        Z = np.concatenate([Z, Y1[Y2 >= (Y1 - 1)**2 / 2]])
    
    Z = Z[:n]
    U = np.random.uniform(0, 1, n)
    Z[U <= 0.5] = -Z[U <= 0.5]
    Z = 1 + 0.1 * Z
    Z = Z[(Z >= 0.5) & (Z <= 1.5)]
    
    while len(Z) < n:
        Y1 = generate_geo(n)
        Y2 = generate_geo(n)
        
        new_Z = Y1[Y2 >= (Y1 - 1)**2 / 2]
        U = np.random.uniform(0, 1, len(new_Z))
        new_Z[U <= 0.5] = -new_Z[U <= 0.5]
        new_Z = 1 + 0.1 * new_Z
        new_Z = new_Z[(new_Z >= 0.5) & (new_Z <= 1.5)]
        
        Z = np.concatenate([Z, new_Z])
    
    return Z[:n]

class Ship:
    def __init__(self, arrival_time):
        self.arrival_time = arrival_time
        self.unload_start = None
        self.unload_time = None
        self.departure_time = None

    def departure(self):
        self.departure_time = self.unload_start + self.unload_time

class Event:
    def __init__(self, event_type, time, ship_id):  # Use ship_id instead of shipid
        self.event_type = event_type #'arrival' or 'departure'
        self.time = time
        self.ship_id = ship_id  # Use ship_id for consistency

def Simulation(day):
    limit = day * 24

    event_queue = []
    ships = []
    global berths 
    berths = [None,None]
    waiting_times=[]

    queue = []
    current_time = 0

    def schedule_event(event_queue, event_type, event_time, ship_id):
        heapq.heappush(event_queue, (event_time, Event(event_type, event_time, ship_id)))
    
    def sim_ships(ships,event_queue,current_time):
        arrival = draw_weibull(1)*24
        schedule_event(event_queue,'arrival',current_time+arrival,len(ships))
        ships.append(Ship(arrival))
        

    def ship_arrival(ship_id,current_time,event_queue):
        global berths
        ship = ships[ship_id]

        if None in berths:
            b_id = berths.index(None)
            berths[b_id] = ship_id
            if berths == [None,None]:
                ship.unload_time = generate_normal(1)[0]/2
                berths = [ship_id,ship_id]
            else:
                ship.unload_time = generate_normal(1)[0]
            ship.unload_start = current_time
            ship.departure()
            schedule_event(event_queue,'departure',ship.departure_time,ship_id)
        else:
            queue.append(ship_id)
            

    def ship_departure(ship_id, current_time, event_queue):
        global berths
        ship = ships[ship_id]
        waiting_time = ship.unload_start - ship.arrival_time
        waiting_times.append(waiting_time)
        berth_idx = berths.index(ship_id)
        berths[berth_idx] = None  
        
        if queue:
            next_ship_id = queue.pop(0)
            ship_arrival(next_ship_id, current_time, event_queue)
        

    first_arrival = draw_weibull(1)[0] * 24
    ships.append(Ship(first_arrival))
    schedule_event(event_queue, 'arrival', first_arrival, 0)

    while event_queue and current_time < limit:
        current_time, current_event = heapq.heappop(event_queue)  # Process next event
        if current_event.event_type == 'arrival':
            ship_arrival(current_event.ship_id, current_time, event_queue)  # Use ship_id
        elif current_event.event_type == 'departure':
            ship_departure(current_event.ship_id, current_time, event_queue)  # Use ship_id
        
        # Schedule the next ship arrival (if within time limit)
        if current_event.event_type == 'arrival':
            next_arrival = current_time + draw_weibull(1)[0] * 24
            if next_arrival < limit:
                ships.append(Ship(next_arrival))
                schedule_event(event_queue, 'arrival', next_arrival, len(ships) - 1)
    
    avg_waiting_time = np.mean(waiting_times) if waiting_times else 0
    max_waiting_time = np.max(waiting_times) if waiting_times else 0
    return avg_waiting_time, max_waiting_time


# Run the simulation for 90 days
avg_wait, max_wait = Simulation(90)
print(f"Average waiting time: {avg_wait} hours")
print(f"Max waiting time: {max_wait} hours")

