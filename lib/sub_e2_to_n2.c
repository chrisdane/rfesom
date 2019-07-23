// C
void sub_e2_to_n2(int *elem2d_n, int *elem2d, double *data_elem2d, int *nod2d_n)
{
    int n = elem2d_n[0];
    int i, elnodes;
    double tmp[nod2d_n] = {0};

	for (i=0; i<n; i++)
        elnodes = elem2d[i];
		tmp[i] = tmp[i] + data_elem2d[elnodes];
}
