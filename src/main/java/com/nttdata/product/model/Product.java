package com.nttdata.product.model;


import java.math.BigDecimal;
import java.time.LocalDateTime;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.springframework.data.annotation.Id;
import org.springframework.data.mongodb.core.mapping.Document;

/**
 * Class: Product. <br/>
 * <b>Bootcamp NTTDATA</b><br/>
 *
 * @author NTTDATA
 * @version 1.0
 *   <u>Developed by</u>:
 *   <ul>
 *   <li>Developer Carlos</li>
 *   </ul>
 * @since 1.0
 */
@Getter
@Setter
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Document(collection = "product")
public class Product {

  @Id
  private String id;
  private String type;
  private String currency;
  private BigDecimal minimumOpeningAmount;
  private BigDecimal minimumAmountPersonalVip;
  private BigDecimal interestRate;
  private BigDecimal maintenanceCommission;
  private Integer monthlyLimitMovement;
  private Integer limitFreeMovements;
  private BigDecimal commissionMovement;
  private Integer specificDayMonthMovement;
  private LocalDateTime dateCreated;
  private LocalDateTime lastUpdated;

}
