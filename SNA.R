# SNA(Social Network Analysis)
# 개인과 집단들 간의 관계를 노드와 링크로 모델링하여 
# 위상구조와 확산, 진화 과정을 계량적으로 분석하는 방법론
# 분류: 집합론적 방법, 그래프 이론을 이용한 방법, 행렬을 이용한 방법 
# 기법: 중심성*, 밀도, 구조적 틈새, 집중도
## *중심성: 연결정도, 근접, 매개, 위세 
# 커뮤니티 수를 측정하는 방법(3): 
# 1) WALKRAP 알고리즘: 일련의 random walk 과정을 통해 커뮤니티 발견, 각 버스택(그래프 꼭지점)를 하나의 커뮤니티로 취급해 병합하며 클러스터링 
# [R] walktrap.community
# 2) Edge Betweenness method: 그래프에 존재하는 최단거리 중 몇개가 그 edge를 거쳐가는지를 이용해 점수 측정, 높은 점수를 갖는 edge가 클러스터를 분리하는 속성을 가진다고 가정 
# [R] edge.betweenness.community
# 3) NetCluster: 계층적 
# 활용방안: 몇 개의 집단으로 구성되며, 집단간의 특징은 무엇이고, 영향력 있는 고객은 누구인지, 시간의 흐름과 고객 상태의 변화에 따라 누가 다음 영향을 받을지를 기반으로 chunk/acquisition prediction, fraud, product recommendation

# 0. 데이터 load
library(NetData); data(studentnets.M182)
# zero edge 정리 
m182_full_nozero_edges <- subset(m182_full_data_frame, (friend_tie > 0 |  social_tie > 0 | task_tie > 0))
head(m182_full_nozero_edges)

library(igraph)
m182_full <- graph.data.frame(m182_full_nozero_edges)

# 각 sub-graph 추출 
m182_friend <- delete.edges(m182_full, E(m182_full)[get.edge.attribute(m182_full, name="friend_tie") == 0])
m182_social <- delete.edges(m182_full, E(m182_full)[get.edge.attribute(m182_full, name="social_tie") == 0])
m182_task   <- delete.edges(m182_full, E(m182_full)[get.edge.attribute(m182_full, name="task_tie") == 0])

friend_layout <- layout.fruchterman.reingold(m182_friend)
plot(m182_friend, layout=friend_layout, edge.arrow.size=.5)
social_layout <- layout.fruchterman.reingold(m182_social)
plot(m182_social, layout=social_layout, edge.arrow.size=.5)
task_layout <- layout.fruchterman.reingold(m182_task)
plot(m182_task, layout=task_layout, edge.arrow.size=.5)

# 3. COMMUNITY DETECTION
# 방향성 제거 
m182_friend_und <- as.undirected(m182_friend, mode='collapse') 
# 격리된 정점 제거 
m182_friend_no_iso <- delete.vertices(m182_friend_und, V(m182_friend_und)[degree(m182_friend_und) == 0])
# => 단일 무방향 연결로 통합되고 분리가 제거되어 정점의 수 감소 

# COMMUNITY DETECTION: WALKTRAP
friend_comm_wt <- walktrap.community(m182_friend_no_iso, steps = 200, modularity = TRUE)#, labels=TRUE)
friend_comm_wt

friend_comm_dend <- as.dendrogram(friend_comm_wt, use.modularity = TRUE)
plot(friend_comm_dend)

# COMMUNITY DETECTION: Edge Betweenness method
friend_comm_eb <- edge.betweenness.community(m182_friend_no_iso)
plot(as.dendrogram(friend_comm_eb))