# MARTINI
Prototype products for MARTINI project

data processing
1 process_ncdf
2 calculate_indicators
3 IndicatorBoundaries

https://martini.p.niva.no/


# MARTINI app deployment

Private deployment for the public repository [martini](https://github.com/NIVANorge/martini). 

You have to manually select the tag of the image, which is automatically deployed when the tag change is committed on github.  

1. Select your image tag from [artifact registry](https://console.cloud.google.com/artifacts/docker/niva-cd/europe-west1/images/martini?project=niva-cd)
2. Update the newTag field for MARTINI in  [kustomization.yaml](https://github.com/NIVANorge/nivacloud-manifests/blob/main/workloads/martini/base/kustomization.yaml) in the [nivacloud-mainifests repository](https://github.com/NIVANorge/nivacloud-manifests)

3. Commit and push
