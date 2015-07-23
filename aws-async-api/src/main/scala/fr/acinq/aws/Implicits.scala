package fr.acinq.aws

import com.amazonaws.regions.{Regions, Region}
import com.amazonaws.services.simpleemail.AmazonSimpleEmailServiceClient

/**
 * Created by PM on 21/07/2015.
 */
package Implicits {

  package object EU_WEST_1 {

      implicit val ses = new AmazonSimpleEmailServiceClient()
      ses.setRegion(Region.getRegion(Regions.EU_WEST_1))

  }


}
