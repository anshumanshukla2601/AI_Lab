#include <iostream>
#include <vector>
#include <queue>
#include <set>
#include <map>
#include <unordered_map>
#include <unordered_set>
#include<algorithm>

using namespace std;

bool checkGoal(vector<int> &curr, vector<int> &goal)
{
    return curr == goal;
}

string encode(vector<int> &curr)
{
    string key = "";
    for (int n : curr)
    {
        key += to_string(n);
    }
    return key;
}

bool isValid(int x, int y, int size, vector<vector<int>> &river) {
    return (x >= 0 && y >= 0 && x < size && y < size && river[x][y] == 0);
}

int h_value(vector<int> &curr, vector<int> &goal)
{
    int x = curr[0], y = curr[1];
    int x1 = goal[0], y1 = goal[1];
    int distance = abs(x - x1) + abs(y - y1);
    return distance;
}

vector<vector<int>> genMove(vector<int> &s, int size, vector<vector<int>>& river)
{
    int row = s[0], col = s[1];
    vector<vector<int>> children;

    if (isValid(row + 1, col, size, river)) 
        children.push_back({row + 1, col});

    if (isValid(row, col + 1, size, river)) 
        children.push_back({row, col + 1});

    if (isValid(row + 1, col + 1, size, river)) 
        children.push_back({row + 1, col + 1});

    if (isValid(row - 1, col, size, river)) 
        children.push_back({row - 1, col});

    if (isValid(row - 1, col + 1, size, river)) 
        children.push_back({row - 1, col + 1});

    if (isValid(row - 1, col - 1, size, river)) 
        children.push_back({row - 1, col - 1});    

    if (isValid(row, col - 1, size, river)) 
        children.push_back({row, col - 1});

    if (isValid(row + 1, col - 1, size, river)) 
        children.push_back({row + 1, col - 1});
    return children;
}


void Path(unordered_map<string, string> &parent, string goalKey, unordered_map<string, vector<int>> &um, 
          int size, vector<vector<int>> &river, vector<int> &initial, vector<int> &goal) {
    vector<vector<int>> path;
    string curr = goalKey;
    while (curr != "0") {
        path.push_back(um[curr]);
        curr = parent[curr];
    }
    reverse(path.begin(), path.end());


    vector<vector<string>> grid(size, vector<string>(size, "."));


    for (int i = 0; i < size; i++) {
        for (int j = 0; j < size; j++) {
            if (river[i][j] == 1) grid[i][j] = "#";
        }
    }


    grid[initial[0]][initial[1]] = "i";
    grid[goal[0]][goal[1]] = "f";

    for (int k = 1; k < path.size(); k++) {
        int x = path[k][0], y = path[k][1];

        if (!(x == goal[0] && y == goal[1])) {
            grid[x][y] = "*";
        }
    }


    cout << "\nPath Matrix:\n";
    for (int i = 0; i < size; i++) {
        for (int j = 0; j < size; j++) {
            cout << grid[i][j] << " ";
        }
        cout << endl;
    }


    cout << "\nPath found:\n";
    for (auto &p : path) {
        cout << "(" << p[0] << "," << p[1] << ") ";
    }
    cout << endl;
}


void solve(vector<int> &initial, vector<int> &goal,int size, vector<vector<int>> &river) {
    struct Node {
        double f, g;
        string key;
        bool operator>(const Node &other) const {
            return f > other.f;
        }
    };

    priority_queue<Node, vector<Node>, greater<Node>> pq;
    unordered_map<string, string> parent;
    unordered_map<string, vector<int>> um;
    unordered_map<string, double> g_cost;
    set<string> closed;

    string start_key = encode(initial);
    um[start_key] = initial;
    parent[start_key] = "0";
    g_cost[start_key] = 0;
    pq.push({(double)h_value(initial, goal), 0.0, start_key});

    while (!pq.empty()) {
        Node curr = pq.top();
        pq.pop();

        vector<int> curr_pos = um[curr.key];
        if (checkGoal(curr_pos, goal)) {
            Path(parent, curr.key, um,size,river,initial,goal);
            return;
        }

        if (closed.find(curr.key) != closed.end()) continue;
        closed.insert(curr.key);

        for (auto &child : genMove(curr_pos, size, river)) {
            string child_key = encode(child);
            double step_cost = (child[0] != curr_pos[0] && child[1] != curr_pos[1]) ? 1.5 : 1.0;
            double new_g = g_cost[curr.key] + step_cost; 
            double f = new_g + h_value(child, goal);

            if (g_cost.find(child_key) == g_cost.end() || new_g < g_cost[child_key]) {
                g_cost[child_key] = new_g;
                parent[child_key] = curr.key;
                um[child_key] = child;
                pq.push({f, new_g, child_key});
            }
        }
    }

    cout << "No path found.\n";
}

int main() {
    int size,size_r;
    cout<<"Enter size of matrix:";
    cin>>size;
    vector<vector<int>> river(size, vector<int>(size, 0));
    int x,y;
    cout<<"Enter coordinates of initial position(indexed from 0):";
    cin>>x>>y;
    vector<int> initial = {x, y};

    cout<<"Enter  coordinates of goal position(indexed from 0):";
    cin>>x>>y;
    vector<int> goal = {x, y};

    cout<<"Enter size of river:";
    cin>>size_r;
    cout<<"Enter points of river:"<<endl;
    for(int i=0; i<size_r; i++){
        cout<<"Point "<<i+1<<":";
        cin>>x>>y;
        river[x][y] = 1;
    }
    solve(initial, goal, size,river);

    return 0;
}