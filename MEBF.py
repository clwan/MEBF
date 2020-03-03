### MEBF python

import numpy as np

n=1
p=0.4
MAT=np.random.binomial(n, p, 9000).reshape(100,90)


def MEBF(MAT,DIM=1000,Thres,COVER=0.95):

    if min(np.shape(MAT))<DIM:
        DIM=min(np.shape(MAT))

    M1 = MAT
    SUM = np.sum(MAT)

    MAT_B = np.empty([np.shape(MAT)[0],0])
    MAT_C = np.empty([0, np.shape(MAT)[1]])

    while np.sum(M1)>(1-COVER)*SUM and min(MAT_B.shape)<DIM:

        C1=0
        B1 = np.zeros(np.shape(M1)[0])
        B1_use = B1
        B2 = np.zeros(np.shape(M1)[1])
        B2_use = B2

        COL = np.sum(M1, axis=0)
        ROW = np.sum(M1, axis=1)

        ### start with column
        if np.median(COL[COL>0])>1:
            TEMP=np.argwhere(COL==min(COL[COL>=np.median(COL[COL>0])]))
            if np.shape(TEMP)[0]==1:
                B1=M1[:,TEMP[0,0]]
                B2[np.sum(M1[B1==1,:],axis=0)>=min(Thres*sum(B1)+1,sum(B1))]=1
                C2=(sum(B1)-1)*(sum(B2)-1)-np.sum(M1[B1==1,:][:,B2==1])

                if C2>C1:
                    B1_use=B1
                    B2_use=B2
                    C1=C2

                B1 = np.zeros(np.shape(M1)[0])
                B2 = np.zeros(np.shape(M1)[1])
            else:
                for j in range(np.shape(TEMP)[0]):
                    B1=M1[:,TEMP[j,0]]
                    B2[np.sum(M1[B1 == 1, :], axis=0) >= min(Thres * sum(B1) + 1, sum(B1))] = 1
                    C2 = (sum(B1) - 1) * (sum(B2) - 1) - np.sum(M1[B1 == 1, :][:, B2 == 1])

                    if C2 > C1:
                        B1_use = B1
                        B2_use = B2
                        C1 = C2

                    B1 = np.zeros(np.shape(M1)[0])
                    B2 = np.zeros(np.shape(M1)[1])


        ### start with row
        if np.median(ROW[ROW>0])>1:
            TEMP = np.argwhere(ROW == min(ROW[ROW >= np.median(ROW[ROW > 0])]))
            if np.shape(TEMP)[0] == 1:
                B2 = M1[TEMP[0,0],:]
                B1[np.sum(M1[:,B2 == 1], axis=1) >= min(Thres * sum(B2) + 1, sum(B2))] = 1
                C2 = (sum(B1) - 1) * (sum(B2) - 1) - np.sum(M1[B1 == 1, :][:, B2 == 1])

                if C2 > C1:
                    B1_use = B1
                    B2_use = B2
                    C1 = C2

                B1 = np.zeros(np.shape(M1)[0])
                B2 = np.zeros(np.shape(M1)[1])
            else:
                for j in range(np.shape(TEMP)[0]):
                    B2 = M1[TEMP[j, 0], :]
                    B1[np.sum(M1[:, B2 == 1], axis=1) >= min(Thres * sum(B2) + 1, sum(B2))] = 1
                    C2 = (sum(B1) - 1) * (sum(B2) - 1) - np.sum(M1[B1 == 1, :][:, B2 == 1])

                    if C2 > C1:
                        B1_use = B1
                        B2_use = B2
                        C1 = C2

                    B1 = np.zeros(np.shape(M1)[0])
                    B2 = np.zeros(np.shape(M1)[1])



        if(C1==0):

            COL_order=np.argsort(COL)[::-1]
            ROW_order=np.argsort(ROW)[::-1]

            B1 = np.zeros(np.shape(M1)[0])
            B2 = np.zeros(np.shape(M1)[1])

            ### start from COL
            B1[(M1[:,COL_order[0]]+M1[:,COL_order[1]]==2)]=1
            B2[np.sum(M1[B1 == 1, :], axis=0) >= min(Thres * sum(B1) + 1, sum(B1))] = 1
            C2 = (sum(B1) - 1) * (sum(B2) - 1) - np.sum(M1[B1 == 1, :][:, B2 == 1])

            if C2>C1:
                B1_use = B1
                B2_use = B2
                C1 = C2


            ### start from ROW
            B1 = np.zeros(np.shape(M1)[0])
            B2 = np.zeros(np.shape(M1)[1])
            B2[(M1[ROW_order[0],] + M1[ROW_order[1],] == 2)] = 1
            B1[np.sum(M1[:, B2 == 1], axis=1) >= min(Thres * sum(B2) + 1, sum(B2))] = 1
            C2 = (sum(B1) - 1) * (sum(B2) - 1) - np.sum(M1[B1 == 1, :][:, B2 == 1])

            if C2>C1:
                B1_use = B1
                B2_use = B2
                C1 = C2

        if C1==0:
            break
        else:
            M1[B1_use==1,:][:,B2_use==1]=0
            B1_use=B1_use.reshape(B1_use.shape[0],1)
            B2_use=B2_use.reshape(1,B2_use.shape[0])
            MAT_B = np.concatenate((MAT_B, B1_use), axis=1)
            MAT_C = np.concatenate((MAT_C, B2_use), axis=0)

    return MAT_B,MAT_C