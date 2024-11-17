import numpy as np
import heapq
import scipy.special as sp
import scipy.stats as stats 

current_docks = 2
cycles = []  
waiting_times = []

'''
Function to draw from a Weibull distribution with a mean of 1.25
'''
mean_weibull = 1.25  # Change this to 0.75 to force more congestion
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
        self.waited = False 
        self.double = False              

    def departure(self):
        self.departure_time = self.unload_start + self.unload_time

class Event:
    def __init__(self, event_type, time, ship_id):
        self.event_type = event_type  # 'arrival' or 'departure'
        self.time = time
        self.ship_id = ship_id

def SimulationWithRegeneration(day):
    global current_docks,cycles, waiting_times
    limit = day * 24  # Total simulation time in hours
    event_queue = []
    ships = []
    
    queue = []  # Queue for ships
    current_time = 0

    def schedule_event(event_type, event_time, ship_id):
        heapq.heappush(event_queue, (event_time, Event(event_type, event_time, ship_id)))

    def ship_arrival(ship_id, current_time):
        global current_docks
        ship = ships[ship_id]
        if current_docks == 2:
            ship.unload_time = generate_normal(1)[0] * 24 / 2
            current_docks = 0
            ship.double = True
        elif current_docks == 1:
            ship.unload_time = generate_normal(1)[0] * 24
            current_docks -= 1
        elif current_docks == 0:
            queue.append(ship_id)
            ship.waited = True
            return
        ship.unload_start = current_time
        ship.departure()
        schedule_event('departure', ship.departure_time, ship_id)

    def ship_departure(ship_id, current_time):
        global current_docks, cycles 
        ship = ships[ship_id]

        if ship.waited:
            waiting_time = ship.unload_start - ship.arrival_time
            waiting_times.append(waiting_time)

        if ship.double:
            current_docks = 2
        else:
            current_docks += 1

        # Process the next ship in the queue, if any
        if queue:
            next_ship_id = queue.pop(0)
            ship_arrival(next_ship_id, current_time)

        
        if current_docks == 2 and not queue and len(waiting_times) !=0 :            
            cycles.append(sum(waiting_times)/len(waiting_times))
            waiting_times.clear()

    # Schedule the first ship arrival
    first_arrival = draw_weibull(1)[0] * 24
    ships.append(Ship(first_arrival))
    schedule_event('arrival', first_arrival, 0)

    
    n = 0
    last_event_time = 0
    while event_queue and current_time < limit:
        n += 1
        current_time, current_event = heapq.heappop(event_queue)
        last_event_time = current_time

        if current_event.event_type == 'arrival':
            ship_arrival(current_event.ship_id, current_time)
            # Schedule the next ship arrival
            next_arrival = current_time + draw_weibull(1)[0] * 24
            if next_arrival < limit:
                ships.append(Ship(next_arrival))
                schedule_event('arrival', next_arrival, len(ships) - 1)
        elif current_event.event_type == 'departure':
            ship_departure(current_event.ship_id, current_time)


    return cycles

def SimulationWithRegenerativeMethod(days):
    cycles = SimulationWithRegeneration(days)

    mean_wait = np.mean(cycles)
    n = len(cycles)
    std_dev = np.std(cycles, ddof=1)  
    standard_error = std_dev / np.sqrt(n)

    # Calculate 95% confidence interval using a t-distribution
    t_critical = stats.t.ppf(0.975, df=n - 1)  # Two-tailed t-distribution with n-1 degrees of freedom
    margin_of_error = t_critical * standard_error
    lower_bound = mean_wait - margin_of_error
    upper_bound = mean_wait + margin_of_error

    return mean_wait, lower_bound, upper_bound

days = 10000  # Total simulation days
mean_wait, ci_lower, ci_upper = SimulationWithRegenerativeMethod(days)

print(f"Average queue length: {mean_wait:.2f}")
print(f"95% confidence interval: ({ci_lower:.2f}, {ci_upper:.2f})")
